package co.topl.stakeholder.providers

import akka.actor.{Actor, ActorPath, Props, Timers}
import akka.util.Timeout
import com.google.common.primitives.Bytes
import co.topl.stakeholder.primitives.ByteArrayWrapper
import co.topl.stakeholder.cases._
import co.topl.stakeholder.components.Serializer
import co.topl.stakeholder.primitives.{ActorRefWrapper, _}
import co.topl.stakeholder.remote.SpecTypes._
import co.topl.stakeholder.remote._
import co.topl.network.NetworkController.ReceivableMessages.{RegisterMessageSpecs, SendToNetwork}
import co.topl.network._
import co.topl.network.message.{Message, MessageSpec}
import co.topl.stakeholder.primitives.Base58
import co.topl.settings.AppSettings

import scala.collection.immutable.ListMap
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.math.BigInt
import scala.util.{Random, Success, Try}

class Router(seed: Array[Byte], inputRef: Seq[ActorRefWrapper], settings: AppSettings)
    extends Actor
    with Types
    with Timers {
  val waitTime = TetraParameters.waitTime
  val slotT = TetraParameters.slotT
  val sk_ecx = TetraParameters.sk_ecx
  val pk_ecx = TetraParameters.pk_ecx
  val numMessageProcessors = TetraParameters.numMessageProcessors
  val tetraMessageSpecs = TetraParameters.tetraMessageSpecs
  val networkController: ActorRefWrapper = inputRef.head
  val peerManager: ActorRefWrapper = inputRef(1)

  implicit val routerRef: ActorRefWrapper = {
    Try(inputRef(2)).toOption match {
      case Some(ref: ActorRefWrapper) => ref
      case None                       => ActorRefWrapper.routerRef(self)
    }
  }
  var coordinatorRef: ActorRefWrapper = _
  Try(inputRef(3)).toOption match {
    case Some(ref: ActorRefWrapper) => coordinatorRef = ref
    case None                       =>
  }

  val serializer: Serializer = new Serializer
  var holders: List[ActorRefWrapper] = List()
  val rng = new Random(BigInt(seed).toLong)
  val fch = new Fch
  val ecx = new Ecx
  var holdersPosition: Map[ActorRefWrapper, (Double, Double)] = Map()
  var distanceMap: Map[(ActorRefWrapper, ActorRefWrapper), Long] = Map()

  var holderMessages: Map[Slot, Map[Long, Map[ActorRefWrapper, Map[BigInt, (ActorRefWrapper, ActorRefWrapper, Any)]]]] =
    Map()
  var holderReady: Map[ActorRefWrapper, Boolean] = Map()
  var globalSlot: Slot = 0
  var localSlot: Slot = 0
  var t0: Long = 0
  var ts: Long = 0
  var roundDone = true
  var firstDataPass = true
  var roundStep = "updateSlot"
  val printSteps = false
  var txRoundCounter = 0
  var maxDelay: Double = 0
  var transactionCounter: Int = 0

  var pathToPeer: Map[ActorPath, (String, PublicKey, Long)] = Map()
  var bootStrapJobs: Set[ActorRefWrapper] = Set()

  var egressRoutees: Seq[akka.actor.ActorRef] = Seq()
  var ingressRoutees: Seq[akka.actor.ActorRef] = Seq()
  var localRoutees: Seq[akka.actor.ActorRef] = Seq()
  var rrc: Int = -1

  def roundRobinCount: Int = {
    if (rrc < localRoutees.size - 1) {
      rrc += 1
    } else {
      rrc = 0
    }
    rrc
  }

  def uuid: String = java.util.UUID.randomUUID.toString
  val delayRng = new Random(BigInt(hash(uuid, serializer).data).toLong)

  var systemTime: Long = System.nanoTime()
  var messageTime: Long = 0

  private case object ActorPathSendTimerKey

  /**
   * Message time for MAC authentication,
   * Use this instead of nanoTime so the time increments
   * properly if successive calls to nanoTime return the same value
   * @return next message time
   */

  def nextMsgTime(): Long =
    System.nanoTime() match {
      case newTime: Long if newTime > systemTime =>
        systemTime = newTime
        messageTime = 0
        systemTime
      case _ =>
        messageTime += 1
        systemTime + messageTime
    }

  /**
   * Sends commands one by one to list of stakeholders
   * @param holders actor list
   * @param command object to be sent
   */
  def sendAssertDone(holders: List[ActorRefWrapper], command: Any): Unit =
    for (holder <- holders) {
      implicit val timeout: Timeout = Timeout(waitTime)
      val future = holder ? command
      val result = Await.result(future, timeout.duration)
      assert(result == "done")
    }

  /**
   * Sends command to stakeholder and waits for response
   * @param holder to send to
   * @param command any command
   */
  def sendAssertDone(holder: ActorRefWrapper, command: Any): Unit = {
    implicit val timeout: Timeout = Timeout(waitTime)
    val future = holder ? command
    val result = Await.result(future, timeout.duration)
    assert(result == "done")
  }

  def routerReceive: Receive = {

    case Flag(ref, value) =>
      if (value == roundStep && holderReady.keySet.contains(ref)) {
        holderReady -= ref
        holderReady += (ref -> true)
      }

    case NextSlot =>
      if (roundDone) globalSlot += 1
      roundDone = false

    case Run =>
      timers.startTimerAtFixedRate(Update, Update, 1.nano)
      coordinatorRef ! NextSlot

    case value: CoordRef =>
      coordinatorRef = value.ref
      sender() ! "done"

    case value: String =>
      if (value == "fence_step") {
        println(roundStep)
        sender() ! "done"
      }

    case ActorPathSendTimerKey => holdersToNetwork()

    case value: SetClock =>
      t0 = value.t0
      sender() ! "done"

    case value: GetTime =>
      globalSlot = ((value.t1 - t0) / slotT).toInt

    case RequestPositionData =>
      sender() ! GetPositionData((holdersPosition, distanceMap))

  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    val outTime = (t1 - t0) * 1.0e-9
    val tString = "%6.6f".format(outTime)
    println("Elapsed time: " + tString + " s")
    result
  }

  def getRefs(from: String, recipient: String): Option[(ActorRefWrapper, ActorRefWrapper)] =
    Try(ActorPath.fromString(from)) match {
      case Success(snd: ActorPath) =>
        Try(ActorPath.fromString(recipient)) match {
          case Success(rec: ActorPath) =>
            holders.find(_.actorPath == snd) match {
              case Some(out1: ActorRefWrapper) =>
                holders.find(_.actorPath == rec) match {
                  case Some(out2: ActorRefWrapper) => Some((out1, out2))
                  case None                        => None
                }
              case None => None
            }
          case _ => None
        }
      case _ => None
    }

  private def messageFromPeer: Receive = { case msg @ Message(spec, Left(msgBytes), Some(remote)) =>
    def processRemoteHolders: Unit =
      spec.parseBytes(msgBytes) match {
        case Success(msg: HoldersType @unchecked) =>
          Try {
            for (string <- msg._1)
              Try(ActorPath.fromString(string)).toOption match {
                case Some(newPath: ActorPath) =>
                  holders.find(_.path == newPath) match {
                    case None =>
                      holders ::= ActorRefWrapper(newPath)
                      pathToPeer += (newPath -> (remote.peerInfo.get.peerSpec.agentName, msg._2, msg._3))
                      SharedData.guiPeerInfo.get(remote.peerInfo.get.peerSpec.agentName) match {
                        case Some(list: List[ActorRefWrapper]) =>
                          val newList = ActorRefWrapper(newPath) :: list
                          SharedData.guiPeerInfo -= remote.peerInfo.get.peerSpec.agentName
                          SharedData.guiPeerInfo += (remote.peerInfo.get.peerSpec.agentName -> newList)
                        case None =>
                          SharedData.guiPeerInfo +=
                            (remote.peerInfo.get.peerSpec.agentName -> List(ActorRefWrapper(newPath)))
                      }
                      println("New holder " + newPath.toString)
                      coordinatorRef ! HoldersFromRemote(holders)
                      updatePeerInfo()
                      if (!holders.forall(_.remote)) holdersToNetwork()
                    case Some(actorRef: ActorRefWrapper) =>
                      if (pathToPeer(actorRef.path)._1 != remote.peerInfo.get.peerSpec.agentName) {
                        if (SharedData.guiPeerInfo.keySet.contains(pathToPeer(actorRef.path)._1))
                          SharedData.guiPeerInfo -= pathToPeer(actorRef.path)._1
                        val key = actorRef.path
                        pathToPeer -= key
                        pathToPeer += (key -> (remote.peerInfo.get.peerSpec.agentName, msg._2, msg._3))
                        SharedData.guiPeerInfo.get(remote.peerInfo.get.peerSpec.agentName) match {
                          case Some(list: List[ActorRefWrapper]) =>
                            val newList = actorRef :: list
                            SharedData.guiPeerInfo -= remote.peerInfo.get.peerSpec.agentName
                            SharedData.guiPeerInfo += (remote.peerInfo.get.peerSpec.agentName -> newList)
                          case None =>
                            SharedData.guiPeerInfo += (remote.peerInfo.get.peerSpec.agentName -> List(actorRef))
                        }
                        if (!holders.forall(_.remote)) holdersToNetwork()
                        println("Updated Peer " + newPath.toString)
                        egressRoutees.foreach(_ ! RouterPeerInfo(pathToPeer, Seq(key), bootStrapJobs, holders))
                        ingressRoutees.foreach(_ ! RouterPeerInfo(pathToPeer, Seq(key), bootStrapJobs, holders))
                      }
                  }
                case None => println("Error: could not parse actor path " + string)
              }
          }.orElse(Try(println("Error: remote holders data not parsed")))
        case _ => println("Error: remote holders data not parsed")
      }
    ingressRoutees.size match {
      case 0 =>
        spec.messageCode match {
          case DiffuseDataSpec.messageCode =>
            spec.parseBytes(msgBytes) match {
              case Success(value: (Mac, Array[Byte]) @unchecked) =>
                Try {
                  val mac = value._1
                  val msgBytes = value._2
                  Try(serializer.diffuseFromBytes(msgBytes)) match {
                    case Success(msg) =>
                      getRefs(msg._1, msg._2) match {
                        case Some((s: ActorRefWrapper, r: ActorRefWrapper)) =>
                          Try(ActorPath.fromString(msg._3)).toOption match {
                            case Some(ref: ActorPath) =>
                              if (!r.remote && !bootStrapJobs.contains(r)) {
                                val msgHash = ByteArrayWrapper(
                                  fch.hash(
                                    Bytes.concat(
                                      serializer.getBytes(mac.time),
                                      msgBytes,
                                      ecx.scalarMult(sk_ecx, pathToPeer(s.actorPath)._2)
                                    )
                                  )
                                )
                                if (msgHash == mac.hash && mac.time > pathToPeer(s.actorPath)._3) {
                                  val peerInfo = pathToPeer(s.actorPath)
                                  pathToPeer -= s.actorPath
                                  pathToPeer += (s.actorPath -> (peerInfo._1, peerInfo._2, mac.time))
                                  r.actorRef ! DiffuseData(msg._4, ActorRefWrapper(ref), msg._5, s)
                                } else {
                                  //println(s"Error: Diffuse MAC failed with code ${Base58.encode(mac.hash.data)} time ${mac.time}")
                                }
                              }
                            case _ => println("Error: Diffuse message path not valid")
                          }
                        case None => println("Error: Diffuse message not parsed")
                      }
                    case _ => println("Error: Diffuse message parse failed")
                  }
                }.orElse(Try(println("Error: Diffuse message not valid")))
              case _ => println("Error: Diffuse message not parsed")
            }
          case HelloSpec.messageCode =>
            spec.parseBytes(msgBytes) match {
              case Success(value: (Mac, Array[Byte]) @unchecked) =>
                Try {
                  val mac = value._1
                  val msgBytes = value._2
                  Try(serializer.helloFromBytes(msgBytes)) match {
                    case Success(msg) =>
                      getRefs(msg._1, msg._2) match {
                        case Some((s: ActorRefWrapper, r: ActorRefWrapper)) =>
                          if (!r.remote && !bootStrapJobs.contains(r)) {
                            val msgHash = ByteArrayWrapper(
                              fch.hash(
                                Bytes.concat(
                                  serializer.getBytes(mac.time),
                                  msgBytes,
                                  ecx.scalarMult(sk_ecx, pathToPeer(s.actorPath)._2)
                                )
                              )
                            )
                            if (msgHash == mac.hash && mac.time > pathToPeer(s.actorPath)._3) {
                              val peerInfo = pathToPeer(s.actorPath)
                              pathToPeer -= s.actorPath
                              pathToPeer += (s.actorPath -> (peerInfo._1, peerInfo._2, mac.time))
                              r.actorRef ! Hello(msg._3, s)
                            } else {
                              //println(s"Error: Hello MAC failed with code ${Base58.encode(mac.hash.data)} time ${mac.time}")
                            }
                          }
                        case None => println("Error: Hello message not parsed")
                      }
                    case _ => println("Error: Hello message parse failed")
                  }
                }.orElse(Try(println("Error: Hello message not valid")))
              case _ => println("Error: Hello message not parsed")
            }
          case RequestBlockSpec.messageCode =>
            spec.parseBytes(msgBytes) match {
              case Success(value: (Mac, Array[Byte]) @unchecked) =>
                Try {
                  val mac = value._1
                  val msgBytes = value._2
                  Try(serializer.requestBlockFromBytes(msgBytes)) match {
                    case Success(msg) =>
                      getRefs(msg._1, msg._2) match {
                        case Some((s: ActorRefWrapper, r: ActorRefWrapper)) =>
                          if (!r.remote && !bootStrapJobs.contains(r)) {
                            val msgHash = ByteArrayWrapper(
                              fch.hash(
                                Bytes.concat(
                                  serializer.getBytes(mac.time),
                                  msgBytes,
                                  ecx.scalarMult(sk_ecx, pathToPeer(s.actorPath)._2)
                                )
                              )
                            )
                            if (msgHash == mac.hash && mac.time > pathToPeer(s.actorPath)._3) {
                              val peerInfo = pathToPeer(s.actorPath)
                              pathToPeer -= s.actorPath
                              pathToPeer += (s.actorPath -> (peerInfo._1, peerInfo._2, mac.time))
                              r.actorRef ! RequestBlock(msg._3, msg._4, s)
                            } else {
                              //println(s"Error: RequestBlock MAC failed with code ${Base58.encode(mac.hash.data)} time ${mac.time}")
                            }
                          }
                        case None => println("Error: RequestBlock message not parsed")
                      }
                    case _ => println("Error: RequestBlock message parse failed")
                  }

                }.orElse(Try(println("Error: RequestBlock message not valid")))
              case _ => println("Error: RequestBlock message not parsed")
            }
          case RequestTineSpec.messageCode =>
            spec.parseBytes(msgBytes) match {
              case Success(value: (Mac, Array[Byte]) @unchecked) =>
                Try {
                  val mac = value._1
                  val msgBytes = value._2
                  Try(serializer.requestTineFromBytes(msgBytes)) match {
                    case Success(msg) =>
                      getRefs(msg._1, msg._2) match {
                        case Some((s: ActorRefWrapper, r: ActorRefWrapper)) =>
                          if (!r.remote && !bootStrapJobs.contains(r)) {
                            val msgHash = ByteArrayWrapper(
                              fch.hash(
                                Bytes.concat(
                                  serializer.getBytes(mac.time),
                                  msgBytes,
                                  ecx.scalarMult(sk_ecx, pathToPeer(s.actorPath)._2)
                                )
                              )
                            )
                            if (msgHash == mac.hash && mac.time > pathToPeer(s.actorPath)._3) {
                              val peerInfo = pathToPeer(s.actorPath)
                              pathToPeer -= s.actorPath
                              pathToPeer += (s.actorPath -> (peerInfo._1, peerInfo._2, mac.time))
                              r.actorRef ! RequestTine(msg._3, msg._4, msg._5, s)
                            } else {
                              //println(s"Error: RequestTine MAC failed with code ${Base58.encode(mac.hash.data)} time ${mac.time}")
                            }
                          }
                        case None => println("Error: RequestTine message not parsed")
                      }
                    case _ => println("Error: RequestTine message parse failed")
                  }

                }.orElse(Try(println("Error: RequestTine message not valid")))
              case _ => println("Error: RequestTine message not parsed")
            }
          case ReturnBlocksSpec.messageCode =>
            spec.parseBytes(msgBytes) match {
              case Success(value: (Mac, Array[Byte]) @unchecked) =>
                Try {
                  val mac = value._1
                  val msgBytes = value._2
                  Try(serializer.returnBlocksFromBytes(msgBytes)) match {
                    case Success(msg) =>
                      getRefs(msg._1, msg._2) match {
                        case Some((s: ActorRefWrapper, r: ActorRefWrapper)) =>
                          if (!r.remote && !bootStrapJobs.contains(r)) {
                            val msgHash = ByteArrayWrapper(
                              fch.hash(
                                Bytes.concat(
                                  serializer.getBytes(mac.time),
                                  msgBytes,
                                  ecx.scalarMult(sk_ecx, pathToPeer(s.actorPath)._2)
                                )
                              )
                            )
                            if (msgHash == mac.hash && mac.time > pathToPeer(s.actorPath)._3) {
                              val peerInfo = pathToPeer(s.actorPath)
                              pathToPeer -= s.actorPath
                              pathToPeer += (s.actorPath -> (peerInfo._1, peerInfo._2, mac.time))
                              r.actorRef ! ReturnBlocks(msg._3, msg._4, s)
                            } else {
                              //println(s"Error: ReturnBlocks MAC failed with code ${Base58.encode(mac.hash.data)} time ${mac.time}")
                            }
                          }
                        case None => println("Error: ReturnBlocks message not parsed")
                      }
                    case _ => println("Error: ReturnBlocks message parse failed")
                  }
                }.orElse(Try(println("Error: ReturnBlocks message not valid")))
              case _ => println("Error: ReturnBlocks message not parsed")
            }
          case SendBlockSpec.messageCode =>
            spec.parseBytes(msgBytes) match {
              case Success(value: (Mac, Array[Byte]) @unchecked) =>
                Try {
                  val mac = value._1
                  val msgBytes = value._2
                  Try(serializer.sendBlockFromBytes(msgBytes)) match {
                    case Success(msg) =>
                      getRefs(msg._1, msg._2) match {
                        case Some((s: ActorRefWrapper, r: ActorRefWrapper)) =>
                          if (!r.remote && !bootStrapJobs.contains(r)) {
                            val msgHash = ByteArrayWrapper(
                              fch.hash(
                                Bytes.concat(
                                  serializer.getBytes(mac.time),
                                  msgBytes,
                                  ecx.scalarMult(sk_ecx, pathToPeer(s.actorPath)._2)
                                )
                              )
                            )
                            if (msgHash == mac.hash && mac.time > pathToPeer(s.actorPath)._3) {
                              val peerInfo = pathToPeer(s.actorPath)
                              pathToPeer -= s.actorPath
                              pathToPeer += (s.actorPath -> (peerInfo._1, peerInfo._2, mac.time))
                              r.actorRef ! SendBlock(msg._3, s)
                            } else {
                              //println(s"Error: SendBlock MAC failed with code ${Base58.encode(mac.hash.data)} time ${mac.time}")
                            }
                          }
                        case None => println("Error: SendBlock message not parsed")
                      }
                    case _ => println("Error: SendBlock message parse failed")
                  }

                }.orElse(Try(println("Error: SendBlock message not valid")))
              case _ => println("Error: SendBlock message not parsed")
            }
          case SendTxSpec.messageCode =>
            spec.parseBytes(msgBytes) match {
              case Success(value: (Mac, Array[Byte]) @unchecked) =>
                Try {
                  val mac = value._1
                  val msgBytes = value._2
                  Try(serializer.sendTxFromBytes(msgBytes)) match {
                    case Success(msg) =>
                      getRefs(msg._1, msg._2) match {
                        case Some((s: ActorRefWrapper, r: ActorRefWrapper)) =>
                          if (!r.remote && !bootStrapJobs.contains(r)) {
                            val msgHash = ByteArrayWrapper(
                              fch.hash(
                                Bytes.concat(
                                  serializer.getBytes(mac.time),
                                  msgBytes,
                                  ecx.scalarMult(sk_ecx, pathToPeer(s.actorPath)._2)
                                )
                              )
                            )
                            if (msgHash == mac.hash && mac.time > pathToPeer(s.actorPath)._3) {
                              val peerInfo = pathToPeer(s.actorPath)
                              pathToPeer -= s.actorPath
                              pathToPeer += (s.actorPath -> (peerInfo._1, peerInfo._2, mac.time))
                              r.actorRef ! SendTx(msg._3, s)
                            } else {
                              //println(s"Error: SendTx MAC failed with code ${Base58.encode(mac.hash.data)} time ${mac.time}")
                            }
                          }
                        case None => println("Error: SendTx paths not valid")
                      }
                    case _ => println("Error: SendTx message parse failed")
                  }
                }.orElse(Try(println("Error: SendTx message not valid")))
              case _ => println("Error: SendTx message not parsed")
            }
          case HoldersFromRemoteSpec.messageCode =>
            processRemoteHolders
          case _ => println("Error: message code did not match any specs")
        }
      case _ =>
        spec.messageCode match {
          case HoldersFromRemoteSpec.messageCode =>
            processRemoteHolders
          case _ =>
            spec.messageCode match {
              case DiffuseDataSpec.messageCode  => ingressRoutees.head ! msg
              case HelloSpec.messageCode        => ingressRoutees(1) ! msg
              case RequestBlockSpec.messageCode => ingressRoutees(2) ! msg
              case RequestTineSpec.messageCode  => ingressRoutees(3) ! msg
              case ReturnBlocksSpec.messageCode => ingressRoutees(4) ! msg
              case SendBlockSpec.messageCode    => ingressRoutees(5) ! msg
              case SendTxSpec.messageCode       => ingressRoutees(6) ! msg
            }
        }
    }
  }

  def holdersToNetwork(): Unit =
    sendToNetwork[HoldersType, HoldersFromRemoteSpec.type](
      HoldersFromRemoteSpec,
      (
        holders.filterNot(_.remote).map(_.path.toString),
        pk_ecx,
        nextMsgTime()
      )
    )

  private def holdersFromLocal: Receive = {
    /** accepts list of other holders from coordinator */
    case HoldersFromLocal(list: List[ActorRefWrapper], printInfo) =>
      val name = settings.network.agentName
      for (holder <- list)
        if (!holders.contains(holder)) {
          holders ::= holder
          if (printInfo) SharedData.guiPeerInfo.get(name) match {
            case Some(list: List[ActorRefWrapper]) =>
              val newList = holder :: list
              SharedData.guiPeerInfo -= name
              SharedData.guiPeerInfo += (name -> newList)
            case None => SharedData.guiPeerInfo += (name -> List(holder))
          }
        }
      for (holder <- holders.filterNot(_.remote))
        if (!holdersPosition.keySet.contains(holder)) {
          holdersPosition += (holder -> (rng.nextDouble() * 180.0 - 90.0, rng.nextDouble() * 360.0 - 180.0))
        }
      timers.startTimerAtFixedRate(ActorPathSendTimerKey, ActorPathSendTimerKey, 10.seconds)
      updatePeerInfo()
      localRoutees.foreach(ref => ref ! HoldersWithPosition(holders.filterNot(_.remote), holdersPosition))
      sender() ! "done"
    case value: HoldersWithPosition =>
      holders = value.list
      holdersPosition = value.pos
  }

  private def messageFromLocal: Receive = {
    case MessageFromLocalToRemote(from, r, command, time) if pathToPeer.keySet.contains(r) && !from.remote =>
      egressRoutees.size match {
        case 0 =>
          val s = from.actorPath
          val msgTime: Long = time match {
            case Some(t) => t
            case None    => nextMsgTime()
          }
          command match {
            case c: DiffuseData =>
              val content: DiffuseDataType = (s.toString, r.toString, c.ref.actorPath.toString, c.sid, c.pks)
              val msgBytes = serializer.diffuseToBytes(content)
              val mac = Mac(
                ByteArrayWrapper(
                  fch.hash(
                    Bytes.concat(
                      serializer.getBytes(msgTime),
                      msgBytes,
                      ecx.scalarMult(sk_ecx, pathToPeer(r)._2)
                    )
                  )
                ),
                msgTime
              )
              sendToNetwork[(Mac, Array[Byte]), DiffuseDataSpec.type](DiffuseDataSpec, (mac, msgBytes), r)
            case c: Hello =>
              val content: HelloDataType = (s.toString, r.toString, c.slot)
              val msgBytes = serializer.helloToBytes(content)
              val mac = Mac(
                ByteArrayWrapper(
                  fch.hash(
                    Bytes.concat(
                      serializer.getBytes(msgTime),
                      msgBytes,
                      ecx.scalarMult(sk_ecx, pathToPeer(r)._2)
                    )
                  )
                ),
                msgTime
              )
              sendToNetwork[(Mac, Array[Byte]), HelloSpec.type](HelloSpec, (mac, msgBytes), r)
            case c: RequestBlock =>
              val content: RequestBlockType = (s.toString, r.toString, c.id, c.job)
              val msgBytes = serializer.requestBlockToBytes(content)
              val mac = Mac(
                ByteArrayWrapper(
                  fch.hash(
                    Bytes.concat(
                      serializer.getBytes(msgTime),
                      msgBytes,
                      ecx.scalarMult(sk_ecx, pathToPeer(r)._2)
                    )
                  )
                ),
                msgTime
              )
              sendToNetwork[(Mac, Array[Byte]), RequestBlockSpec.type](RequestBlockSpec, (mac, msgBytes), r)
            case c: RequestTine =>
              val content: RequestTineType = (s.toString, r.toString, c.id, c.depth, c.job)
              val msgBytes = serializer.requestTineToBytes(content)
              val mac = Mac(
                ByteArrayWrapper(
                  fch.hash(
                    Bytes.concat(
                      serializer.getBytes(msgTime),
                      msgBytes,
                      ecx.scalarMult(sk_ecx, pathToPeer(r)._2)
                    )
                  )
                ),
                msgTime
              )
              sendToNetwork[(Mac, Array[Byte]), RequestTineSpec.type](RequestTineSpec, (mac, msgBytes), r)
            case c: ReturnBlocks =>
              val content: ReturnBlocksType = (s.toString, r.toString, c.blocks, c.job)
              val msgBytes = serializer.returnBlocksToBytes(content)
              val mac = Mac(
                ByteArrayWrapper(
                  fch.hash(
                    Bytes.concat(
                      serializer.getBytes(msgTime),
                      msgBytes,
                      ecx.scalarMult(sk_ecx, pathToPeer(r)._2)
                    )
                  )
                ),
                msgTime
              )
              sendToNetwork[(Mac, Array[Byte]), ReturnBlocksSpec.type](ReturnBlocksSpec, (mac, msgBytes), r)
            case c: SendBlock =>
              val content: SendBlockType = (s.toString, r.toString, c.block)
              val msgBytes = serializer.sendBlockToBytes(content)
              val mac = Mac(
                ByteArrayWrapper(
                  fch.hash(
                    Bytes.concat(
                      serializer.getBytes(msgTime),
                      msgBytes,
                      ecx.scalarMult(sk_ecx, pathToPeer(r)._2)
                    )
                  )
                ),
                msgTime
              )
              sendToNetwork[(Mac, Array[Byte]), SendBlockSpec.type](SendBlockSpec, (mac, msgBytes), r)
            case c: SendTx =>
              val content: SendTxType = (s.toString, r.toString, c.transaction)
              val msgBytes = serializer.sendTxToBytes(content)
              val mac = Mac(
                ByteArrayWrapper(
                  fch.hash(
                    Bytes.concat(
                      serializer.getBytes(msgTime),
                      msgBytes,
                      ecx.scalarMult(sk_ecx, pathToPeer(r)._2)
                    )
                  )
                ),
                msgTime
              )
              sendToNetwork[(Mac, Array[Byte]), SendTxSpec.type](SendTxSpec, (mac, msgBytes), r)
            case _ =>
          }
        case _ =>
          command match {
            case _: DiffuseData =>
              egressRoutees.head ! MessageFromLocalToRemote(from, r, command, time = Some(nextMsgTime()))
            case _: Hello =>
              egressRoutees(1) ! MessageFromLocalToRemote(from, r, command, time = Some(nextMsgTime()))
            case _: RequestBlock =>
              egressRoutees(2) ! MessageFromLocalToRemote(from, r, command, time = Some(nextMsgTime()))
            case _: RequestTine =>
              egressRoutees(3) ! MessageFromLocalToRemote(from, r, command, time = Some(nextMsgTime()))
            case _: ReturnBlocks =>
              egressRoutees(4) ! MessageFromLocalToRemote(from, r, command, time = Some(nextMsgTime()))
            case _: SendBlock =>
              egressRoutees(5) ! MessageFromLocalToRemote(from, r, command, time = Some(nextMsgTime()))
            case _: SendTx =>
              egressRoutees(6) ! MessageFromLocalToRemote(from, r, command, time = Some(nextMsgTime()))
            case _ =>
          }
      }
    case TineProvider.Egress(content) =>
      content match {
        case Left(msg: MessageFromLocalToRemote) =>
          self ! msg
          sender() ! "done"
        case Right(msg: MessageFromLocalToLocal) =>
          self ! msg
          sender() ! "done"
      }
  }

  private def sendToNetwork[Content, Spec <: MessageSpec[Content]](spec: Spec, c: Content, r: ActorPath): Unit =
    Try(spec.toBytes(c)).toOption match {
      case Some(bytes: Array[Byte]) =>
        pathToPeer.get(r) match {
          case Some((peerName, _, _)) =>
            networkController ! SendToNetwork(
              Message(spec, Left(bytes), None),
              SendToPeerByName(peerName, self)
            )
          case None =>
        }
      case None =>
    }

  private def sendToNetwork[Content, Spec <: MessageSpec[Content]](spec: Spec, c: Content): Unit =
    Try(spec.toBytes(c)).toOption match {
      case Some(bytes: Array[Byte]) =>
        networkController ! SendToNetwork(Message(spec, Left(bytes), None), BroadcastExceptOfByName("bootstrap"))
      case None =>
    }

  private def registerNC: Receive = {
    case InvalidateHolders(peerName) if pathToPeer.map(p => p._2._1).toArray.contains(peerName) =>
      var holdersOut: List[ActorRefWrapper] = holders.filterNot(_.remote)
      var holdersToRemove: Seq[ActorPath] = Seq()
      for (holder <- holders) if (pathToPeer.keySet.contains(holder.actorPath)) {
        if (pathToPeer(holder.actorPath)._1 == peerName) {
          pathToPeer -= holder.actorPath
          holdersToRemove ++= Seq(holder.actorPath)
        } else {
          holdersOut ::= holder
        }
      }
      egressRoutees.foreach(_ ! RouterPeerInfo(pathToPeer, holdersToRemove, bootStrapJobs, holders))
      ingressRoutees.foreach(_ ! RouterPeerInfo(pathToPeer, holdersToRemove, bootStrapJobs, holders))
      //println("Peer removed: "+peerName+", Number of peers: "+holders.count(_.remote).toString)
      holders = holdersOut
      coordinatorRef ! HoldersFromRemote(holders)
    case Populate =>
      var i = 0
      localRoutees = Seq.fill(numMessageProcessors) {
        val ref = context.actorOf(
          Router.props(
            fch.hash("%sloc%d".format(seed, i)),
            inputRef.map(_.actorRef) ++ Seq(self, coordinatorRef.actorRef),
            settings
          ),
          s"localRoutee_$i"
        )
        i += 1
        ref
      }
      sender() ! "done"
    case Register =>
      networkController ! RegisterMessageSpecs(tetraMessageSpecs, self)
      if (TetraParameters.useRouterSystem) {
        var i = 0
        egressRoutees = Seq.fill(7) {
          val ref = context.actorOf(
            Router.props(
              fch.hash("%segr%d".format(seed, i)),
              inputRef.map(_.actorRef) ++ Seq(self, coordinatorRef.actorRef),
              settings
            ),
            s"egressRoutee_$i"
          )
          i += 1
          ref
        }
        i = 0
        ingressRoutees = Seq.fill(7) {
          val ref = context.actorOf(
            Router.props(
              fch.hash("%sing%d".format(seed, i)),
              inputRef.map(_.actorRef) ++ Seq(self, coordinatorRef.actorRef),
              settings
            ),
            s"ingressRoutee_$i"
          )
          i += 1
          ref
        }
        println("Router System Started...")
      }
      sender() ! "done"
    case BootstrapJob(bootStrapper) =>
      if (bootStrapJobs.contains(bootStrapper)) {
        bootStrapJobs -= bootStrapper
      } else {
        bootStrapJobs += bootStrapper
      }
      updatePeerInfo()
  }

  def updatePeerInfo(): Unit = {
    egressRoutees.foreach(_ ! RouterPeerInfo(pathToPeer, Seq(), bootStrapJobs, holders))
    ingressRoutees.foreach(_ ! RouterPeerInfo(pathToPeer, Seq(), bootStrapJobs, holders))
  }

  def syncPeerInfo: Receive = { case value: RouterPeerInfo =>
    for (entry <- value.pathsToRemove)
      if (pathToPeer.keySet.contains(entry)) pathToPeer -= entry
    for (entry <- value.pathToPeer)
      if (!pathToPeer.keySet.contains(entry._1)) pathToPeer += (entry._1 -> entry._2)
    bootStrapJobs = value.bootStrapJobs
    holders = value.holders
  }

  def receive: Receive =
    registerNC orElse
    syncPeerInfo orElse
    holdersFromLocal orElse
    messageFromLocal orElse
    messageFromPeer orElse
    routerReceive orElse { case _ =>
    }
}

object Router {

  def props(seed: Array[Byte], ref: Seq[akka.actor.ActorRef], settings: AppSettings): Props =
    Props(new Router(seed, ref.map(ActorRefWrapper.routerRef), settings)).withDispatcher(TetraParameters.routerEC)
}
