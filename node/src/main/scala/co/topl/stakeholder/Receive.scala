package co.topl.stakeholder

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import co.topl.stakeholder.primitives.{ActorRefWrapper, Kes, KeyFile, Ratio, SharedData, Sig, Vrf}
import co.topl.stakeholder.providers.TineProvider
import co.topl.stakeholder.primitives.ByteArrayWrapper

import scala.concurrent.duration._
import co.topl.stakeholder.cases._
import co.topl.stakeholder.components.{Block, Tine, Transaction}
import co.topl.stakeholder.primitives.Base58

import scala.math.BigInt
import scala.util.Random
import scala.util.{Failure, Success, Try}

/**
 * AMS 2020:
 * The receive statement of Stakeholder,
 * Contains cases of all messages Stakeholder will encounter from local and remote interfaces
 * Message authentication occurs for Block passing and TinePool messages
 */

trait Receive extends Members {

  def receive: Receive = {

    /**
     * Updates time, the kes key, and resets variables
     * Primary runtime loop for consensus
     */
    case Update =>
      t1 = globalTime
      globalSlot = ((t1 - t0) / slotT).toInt
      update()
      timers.startSingleTimer(Update, Update, updateTime)

    /**
     * ****************************************** Network Messages ********************************************
     */

    /**
     * Primary transaction passing method, each Tx signature is validated and the Tx is statefully checked
     * Newly encountered Txs are sent to gossipers
     */

    case value: SendTx =>
      Try {
        if (!memPool.keySet.contains(value.transaction.sid) && localState.keySet.contains(value.transaction.sender)) {
          if (localState(value.transaction.sender)._3 <= value.transaction.nonce) {
            if (verifyTransaction(value.transaction)) {
              if (!bootStrapLock) memPool += (value.transaction.sid -> (value.transaction, 0))
              send(selfWrapper, gossipSet(selfWrapper, value.sender, holders), SendTx(value.transaction, selfWrapper))
            }
          }
        }
      }

    /**
     * Block passing for newly forged blocks only,
     * If block is not found in database it is added to the tinepool as a new open tine and then sent to gossipers
     * Note AMS 2020:
     * This is where tines are introduced to the tinepool,
     * blocks below the checkpoint depth and blocks from the future never enter tinepool
     * if bootstrapping no new tines are made until the job is done or the connection is lost,
     * when bootstrapping new blocks are added to database but do not enter tinepool
     */

    case value: SendBlock =>
      Try {
        val foundBlock = blocks.knownInCache((value.block.slot, value.block.id))
        if (!foundBlock) {
          val b: BlockHeader = value.block.tetraHeader
          val bHash = hash(b, serializer)
          val bSlot = b._3
          val bRho = b._5
          if (holderIndex == SharedData.printingHolder && !helloLock) {
            val newDelay = (t1 - t0 - bSlot * slotT).toDouble / 1000.0
            if (newDelay > 0.0) networkDelayList ::= newDelay
            if (networkDelayList.size > 100) networkDelayList.take(100)
            SharedData.maxNetworkDelay = Array(newDelay, SharedData.maxNetworkDelay).max
            if (SharedData.minNetworkDelay == 0.0) {
              if (newDelay > 0.0) SharedData.minNetworkDelay = newDelay
            } else {
              if (newDelay > 0.0) SharedData.minNetworkDelay = Array(newDelay, SharedData.minNetworkDelay).min
            }
            SharedData.averageNetworkDelay = average(networkDelayList)
          }
          if (verifyBlock(value.block)) {
            blocks.add(value.block)
            if (bSlot <= globalSlot && bSlot > globalSlot - slotWindow) {
              val newId = (bSlot, bHash)
              val currentHead = getBlockHeader(localChain.head).get
              if (
                value.block.number > currentHead._9
                || value.block.slot < currentHead._3
                && value.block.number == currentHead._9
                && useMaxValidTK
              ) {
                send(selfWrapper, gossipSet(selfWrapper, value.sender, holders), SendBlock(value.block, selfWrapper))
              }
              if (!bootStrapLock) {
                if (tinePool.keySet.size > tineMaxTries) {
                  println("Holder " + holderIndex.toString + " Dropping Tine")
                  tinePool -= tinePool.keySet.min
                } else {
                  for (entry <- tinePool)
                    if (entry._2._1.head._1 <= globalSlot - slotWindow || !holders.contains(entry._2._5)) {
                      println("Holder " + holderIndex.toString + " Dropping Tine")
                      tinePool -= entry._1
                    }
                }
                val jobNumber = tineCounter
                tinePool += (jobNumber -> (Tine(newId, bRho), 0, 0, 0, value.sender))
                buildTine((jobNumber, tinePool(jobNumber)))
                tineCounter += 1
              }
            }
          } else { println("error: invalid block") }
        }
      }

    /**
     * Block passing for tinepool functionality, returned blocks are added to block database
     */

    case value: ReturnBlocks =>
      Try {
        def blockToId(b: Block): SlotId = (b.slot, b.id)
        for (block <- value.blocks)
          if (value.job >= 0 && tinePool.keySet.contains(value.job) && !helloLock) {
            if (bootStrapLock) {
              if (!blocks.knownIfPresent(blockToId(block))) {
                if (verifyBlock(block)) {
                  println("Holder " + holderIndex.toString + " Got Block " + Base58.encode(block.id.data))
                  blocks.add(block)
                } else { println("Error: invalid returned block") }
              }
              if (value.job == bootStrapJob) {
                timers.startSingleTimer(BootstrapJob, BootstrapJob, 10 * slotT.millis)
              }
            } else {
              if (!blocks.knownInCache(blockToId(block))) {
                if (verifyBlock(block)) {
                  println("Holder " + holderIndex.toString + " Got Block " + Base58.encode(block.id.data))
                  blocks.add(block)
                } else { println("Error: invalid returned block") }
              }
              buildTine((value.job, tinePool(value.job)))
            }
          } else if (value.job == -1 && helloLock) {
            if (!blocks.knownInCache(blockToId(block))) {
              if (verifyBlock(block)) {
                println("Holder " + holderIndex.toString + " Got Block " + Base58.encode(block.id.data))
                blocks.add(block)
              } else { println("Error: invalid returned block") }
            }
            val b = block.tetraHeader
            val bHash = hash(b, serializer)
            val bSlot = b._3
            val bRho = b._5
            if (tinePool.keySet.contains(-1)) tinePool -= -1
            tinePoolWithPrefix = Array()
            if (tinePool.keySet.isEmpty) {
              tinePool += (-1 -> (Tine((bSlot, bHash), bRho), 0, 0, 0, value.sender))
            }
            if (tinePool.keySet.contains(-1)) buildTine((-1, tinePool(-1)))
            bootstrapSelectTine()
            timers.startSingleTimer(BootstrapJob, BootstrapJob, 10 * slotT.millis)
          }
      }

    /**
     * Block requesting for tinepool functionality, parent ids that are not found are requested from peers
     */

    case value: RequestBlock =>
      Try {
        println("Holder " + holderIndex.toString + " Was Requested Block")
        blocks.getIfInCache(value.id) match {
          case Some(returnedBlock: Block) =>
            send(selfWrapper, value.sender, ReturnBlocks(List(returnedBlock), value.job, selfWrapper))
            println("Holder " + holderIndex.toString + " Returned Block")
          case None =>
        }
      }

    /**
     * Block requesting for tinepool functionality, parent ids are requested with increasing
     * depth of chain up to a finite number of attempts
     * this message is sent as a result of a tine in tinepool becoming long enough to trigger bootstrapping mode,
     * Spins up a provider to search database for blocks
     */

    case value: RequestTine =>
      Try {
        tineProvider match {
          case None =>
            if (value.depth <= tineMaxDepth) {
              val startId: SlotId = value.id
              val depth: Int = value.depth
              tineProvider = Try {
                ActorRefWrapper(context.actorOf(TineProvider.props(blocks, localRef, settings), "RequestTineProvider"))
              }.toOption
              tineProvider match {
                case Some(ref: ActorRefWrapper) =>
                  ref ! TineProvider.Info(
                    holderIndex,
                    value.sender,
                    selfWrapper,
                    startId,
                    depth,
                    value.job,
                    None,
                    None
                  )
                case None => println("error: tine provider not initialized")
              }
            } else { println("error: chain request mac invalid") }
          case _ =>
        }
      }

    case TineProvider.Done =>
      tineProvider = None

    /**
     * Greeting message for bootstrapping nodes,
     * contains the last known slot, triggers tine recovery up to current head
     */

    case value: Hello =>
      Try {
        tineProvider match {
          case None =>
            val startId: SlotId = localChain.getLastActiveSlot(value.slot).get
            val depth: Int = tineMaxDepth
            tineProvider = Try {
              ActorRefWrapper(context.actorOf(TineProvider.props(blocks, localRef, settings), "BootstrapProvider"))
            }.toOption
            tineProvider match {
              case Some(ref: ActorRefWrapper) =>
                ref ! TineProvider.Info(
                  holderIndex,
                  value.sender,
                  selfWrapper,
                  startId,
                  depth,
                  -1,
                  Some(localChain.getNext(value.slot, depth)),
                  Some(inbox)
                )
              case None => println("Error: tine provider not initialized")
            }
          case _ =>
        }
      }

    /**
     * Validates diffused keys from other holders and stores in inbox
     */

    case value: DiffuseData =>
      if (!inbox.keySet.contains(value.sid)) {
        val pkwNew = ByteArrayWrapper(value.pks._1 ++ value.pks._2 ++ value.pks._3)
        if (pkwNew != keys.pkw) {
          for (entry <- inbox) {
            val pkwEntry = ByteArrayWrapper(entry._2._2._1 ++ entry._2._2._2 ++ entry._2._2._3)
            if (pkwEntry == pkwNew) inbox -= entry._1
          }
          inbox += (value.sid -> (value.ref, value.pks))
          send(
            selfWrapper,
            gossipSet(selfWrapper, value.sender, holders),
            DiffuseData(value.sid, value.ref, value.pks, selfWrapper)
          )
        }
      }

    /**
     * ****************************************** From Local ***************************************************
     */

    case DelayModelMessage(delay, nonce, msg) =>
      timers.startSingleTimer(nonce, msg, delay)

    case BootstrapJob =>
      if (helloLock) {
        if (tinePool.keySet.isEmpty && tinePoolWithPrefix.isEmpty) {
          val lastSlot = localChain.head._1
          if (globalSlot > 1 && lastSlot < globalSlot - tineBootstrappingDepth * 4) {
            println(s"Current Head slot $lastSlot < ${globalSlot - tineBootstrappingDepth * 4}")
            println(s"Holder $holderIndex Bootstrapping...")
            send(
              selfWrapper,
              gossipSet(selfWrapper, holders).take(1),
              Hello(lastSlot + 1, selfWrapper)
            )
            chainStorage.store(localChain, dataBaseCID, serializer)
            timers.startSingleTimer(BootstrapJob, BootstrapJob, 10 * slotT.millis)
          } else {
            bootStrapLock = false
            helloLock = false
          }
        } else {
          if (tinePool.keySet.contains(-1)) buildTine((-1, tinePool(-1)))
          bootstrapSelectTine()
          self ! BootstrapJob
        }
      } else if (bootStrapLock && tinePool.keySet.contains(bootStrapJob)) {
        println(s"Holder $holderIndex Bootstrapping...")
        if (holders.contains(tinePool(bootStrapJob)._5)) {
          buildTine((bootStrapJob, tinePool(bootStrapJob)))
        } else {
          println(s"Holder $holderIndex Lost Connection with Tine Provider")
          tinePool -= bootStrapJob
          bootStrapJob = -1
          bootStrapLock = false
        }
      } else {
        println(s"Holder $holderIndex Bootstrap Job not in Tinepool")
        bootStrapJob = -1
        bootStrapLock = false
      }

    /** sends holder information for populating inbox */
    case Diffuse =>
      send(
        selfWrapper,
        gossipSet(selfWrapper, holders),
        DiffuseData(hash(rng.nextString(8), serializer), selfWrapper, keys.publicKeys, selfWrapper)
      )

    /** allocation and vars of simulation */
    case Initialize(gs, inputPassword) =>
      globalSlot = if (gs > 0) { gs }
      else { 0 }
      println("Holder " + holderIndex.toString + s" setting up")
      println("Configuring key...")
      time {
        inputPassword match {
          case Some(pw)               => password = pw
          case None if password == "" => password = s"password_holder_$holderIndex"
          case _                      =>
        }
        salt = fch.hash(uuid)
        derivedKey = KeyFile.getDerivedKey(password, salt)

        def generateNewKeys(): Unit = {
          println("Generating new keyfile...")
          val rngSeed: Random = new Random
          rngSeed.setSeed(BigInt(seed).toLong)
          val seed1 = fch.hash(rngSeed.nextString(32))
          val seed2 = fch.hash(rngSeed.nextString(32))
          val seed3 = fch.hash(rngSeed.nextString(32))
          keyFile = Some(
            KeyFile.fromSeed(
              password,
              s"$storageDir/keys/",
              serializer,
              sig: Sig,
              vrf: Vrf,
              kes: Kes,
              0,
              seed1,
              seed2,
              seed3
            )
          )
        }

        keyFile match {
          case None =>
            Try(KeyFile.restore(s"$storageDir/keys/")) match {
              case Success(Some(restoredFile: KeyFile)) =>
                println("Reading Keyfile")
                keyFile = Some(restoredFile)
              case Success(None) => generateNewKeys()
              case Failure(exception) =>
                exception.printStackTrace()
                generateNewKeys()
            }
          case _ =>
        }
        keys = keyFile.get.getKeys(password, serializer, sig, vrf, kes)
      }
      println("Setting up local chain...")
      time {
        chainStorage.restore(dataBaseCID, serializer) match {
          case None =>
            localChain.loadCache()
            localChain.update((0, genBlockHash), genesisBlock.get.blockHeader.get._5)
            updateLocalState(localState, (0, genBlockHash)) match {
              case Some(value: State) => localState = value
              case _ =>
                SharedData.throwError(holderIndex)
                println("error: invalid genesis block")
            }
            eta = eta_from_genesis(localChain, 0)
            println(s"Eta 0: ${Base58.encode(eta)}")
            println("Adding genesis state to history")
            history.add((0, genBlockHash), localState, eta)
            history.cacheStakeDist((0, genBlockHash))
            stakingState = getStakingState(currentEpoch, localChain, None)
            alphaCache match {
              case Some(loadingCache: LoadingCache[ByteArrayWrapper, Ratio]) =>
                loadingCache.invalidateAll()
              case None =>
                alphaCache = Some(
                  CacheBuilder
                    .newBuilder()
                    .build[ByteArrayWrapper, Ratio](
                      new CacheLoader[ByteArrayWrapper, Ratio] {
                        def load(id: ByteArrayWrapper): Ratio = relativeStake(id, stakingState)
                      }
                    )
                )
            }
            updateWallet()
          case Some(newChain: Tine) =>
            localChain.loadCache()
            localChain.copy(newChain)
            localChain.populateCache()
            val headId = localChain.head
            localSlot = headId._1
            val newHead = getBlockHeader(headId)
            println(
              Console.CYAN + "Local Slot = " + localSlot.toString + s" on block ${newHead.get._9} "
              + Base58.encode(headId._2.data) + Console.RESET
            )
            currentEpoch = localSlot / epochLength
            val loadState = history.get(headId).get
            localState = loadState._1
            eta = loadState._2
            stakingState = getStakingState(currentEpoch, localChain, None)
            alphaCache match {
              case Some(loadingCache: LoadingCache[ByteArrayWrapper, Ratio]) =>
                loadingCache.invalidateAll()
              case None =>
                alphaCache = Some(
                  CacheBuilder
                    .newBuilder()
                    .build[ByteArrayWrapper, Ratio](
                      new CacheLoader[ByteArrayWrapper, Ratio] {
                        def load(id: ByteArrayWrapper): Ratio = relativeStake(id, stakingState)
                      }
                    )
                )
            }
            updateWallet()
        }
      }
      println("Setting relative stake...")
      time {
        keys.alpha = alphaCache.get.get(keys.pkw)
      }
      assert(genBlockHash == hash(genesisBlock.get.blockHeader.get, serializer))
      println("Initialization Complete.")
      sender() ! "done"

    /** starts the timer that repeats the update command */
    case Run =>
      timers.startSingleTimer(Update, Update, updateTime)
      timers.startSingleTimer(Refresh, Refresh, slotT * (refreshInterval * rng.nextDouble).toInt.millis)
      scheduleDiffuse()
      self ! BootstrapJob

    case Refresh =>
      blocks.refresh()
      history.refresh()
      timers.startTimerWithFixedDelay(Refresh, Refresh, slotT * refreshInterval.millis)

    case GetTime =>
      syncGlobalClock()

    /** sets the initial time */
    case value: SetClock =>
      t0 = value.t0
      sender() ! "done"

    /** sets the slot from coordinator time */
    case value: GetTime =>
      t1 = value.t1
      globalSlot = ((value.t1 - t0) / slotT).toInt

    /** accepts list of other holders from coordinator */
    case HoldersFromLocal(list: List[ActorRefWrapper], _) =>
      holders = list
      sender() ! "done"

    /** accepts genesis block from coordinator */
    case gb: GenBlock =>
      genBlockHash = hash(gb.b.tetraHeader, serializer)
      println("Holder " + holderIndex.toString + " got genesis block " + Base58.encode(genBlockHash.data))
      assert(genBlockHash == gb.b.id)
      assert(verifyBlock(gb.b))
      genesisBlock = Some(gb.b)
      if (!blocks.knownIfPresent((0, gb.b.id))) {
        blocks.add(gb.b)
      }
      sender() ! "done"

    /** accepts coordinator ref */
    case CoordRef(ref) =>
      coordinatorRef = ref
      sender() ! "done"

    /** sets new list of holders resets gossipers */
    case Party(list, clear) =>
      holders = list
      if (clear) inbox = Map()
      sender() ! "done"

    /**
     * *********************************** Testing *****************************************
     */

    /** prints inbox */
    case Inbox =>
      var i = 0
      println("Holder " + holderIndex.toString + " sid:" + Base58.encode(sessionId.data) + ", Inbox:")
      for (entry <- inbox) {
        println(i.toString + " " + Base58.encode(entry._1.data))
        i += 1
      }
      println("Known holders:")
      holders.foreach(r => println(r.actorPath.toString))
      sender() ! "done"

    /** prints stats */
    case Verify =>
      val trueChain = verifyChain(localChain, genBlockHash)
      println(
        "Holder " + holderIndex.toString + ": t = " + localSlot.toString + ", alpha = " + keys.alpha.toDouble
        + "\nChain length = " + localChain.numActive.toString + ", Valid chain = "
        + trueChain.toString
      )
      var chainBytes: Array[Byte] = Array()
      for (id <- localChain.slice(0, localSlot).ordered)
        getBlockHeader(id) match {
          case Some(b: BlockHeader) => chainBytes ++= fch.hash(serializer.getBytes(b))
          case _                    =>
        }
      println("Public Key: " + Base58.encode(keys.pk_sig ++ keys.pk_vrf ++ keys.pk_kes))
      println("Path: " + self.path)
      println("Chain hash: " + Base58.encode(fch.hash(chainBytes)) + "\n")
      if (SharedData.error) {
        for (id <- localChain.ordered)
          if (id._1 > -1)
            println("H:" + holderIndex.toString + "S:" + id._1.toString + "ID:" + Base58.encode(id._2.data))
        println("e:" + Base58.encode(eta_from_genesis(localChain, currentEpoch)) + "\n")
      }
      sender() ! "done"

    /** prints stats */
    case Status =>
      println(
        "Holder " + holderIndex.toString + ": t = " + localSlot.toString + ", alpha = " + keys.alpha.toDouble
        + "\nChain length = " + localChain.numActive.toString + ", MemPool Size = " + memPool.size
      )
      var chainBytes: Array[Byte] = Array()
      for (id <- localChain.slice(0, localSlot).ordered)
        getBlockHeader(id) match {
          case Some(b: BlockHeader) =>
            chainBytes ++= fch.hash(serializer.getBytes(b))
          case _ =>
        }
      var txCount = 0
      var allTx: List[Sid] = List()
      var duplicatesFound = false
      var allTxSlots: List[Slot] = List()
      var holderTxOnChain: List[(Sid, Transaction)] = List()
      for (id <- localChain.slice(1, localSlot).ordered)
        for (trans <- blocks.getIfPresent(id).get.blockBody.get)
          if (!allTx.contains(trans.sid)) {
            if (trans.sender == keys.pkw) holderTxOnChain ::= (trans.sid, trans)
            allTx ::= trans.sid
            allTxSlots ::= id._1
            txCount += 1
          } else {
            duplicatesFound = true
          }
      val holderTxCount = holderTxOnChain.length
      val txCountChain = if (holderTxOnChain.isEmpty) { 0 }
      else { holderTxOnChain.head._2.nonce }
      val txCountState = math.max(localState(keys.pkw)._3 - 1, 0)
      println(s"Tx Counts in state and chain: $txCountState, $txCountChain")
      println(s"Transactions on chain: $holderTxCount/$txCount Duplicates: $duplicatesFound")
      println("Chain hash: " + Base58.encode(fch.hash(chainBytes)) + "\n")
      sender() ! "done"

    case unknown: Any =>
      print("Error: received unknown message ")
      println(unknown.getClass.toString)

  }
}
