package co.topl.networking.p2p

import akka.stream.scaladsl.{Flow, Keep, Source}
import akka.stream.stage._
import akka.stream.{Attributes, FlowShape, Inlet, Outlet}
import akka.util.ByteString
import cats.implicits._
import co.topl.crypto.hash.Blake2b256
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models.{Bytes, Evidence}
import co.topl.networking.multiplexer.{bytestringToInt, intToBytestring}

import scala.concurrent.{Future, Promise}
import scala.util.Random

/**
 * An Akka-Stream Flow which performs a pre-handshake before materializing the given sub-flow.  This mini-handshake
 * simply establishes a "Connection Leader" with which the given `f` function is applied.  The "Connection Leader" is
 * established by each party sending a random number to the other.  The party with the larger number is deemed to be
 * the "Connection Leader" which can be interpreted as needed when forming the child sub-flow.
 *
 * Each party locally selects an integer at random.  Next, each party sends a hash of their local number to the other.
 * Next, the parties exchange the actual numbers.  Each party locally verifies the commitment made by the other.  Next,
 * the "Connection Leader" is determined by:
 * - if(
 *      BigInt(hash(localNumber.bytes ++ remoteNumber.bytes)) >
 *        BigInt(hash(remoteNumber.bytes ++ localNumber.bytes))
 *    ) ConnectionLeaders.Local
 *   else ConnectionLeaders.Remote
 *
 * If the hash where the localNumber's bytes came first "wins", then the local party is the "Connection Leader"
 *
 * TODO: Consider a key exchange mechanism
 */
object ConnectionLeaderFlow {

  def apply[Mat](
    f:               ConnectionLeader => Flow[ByteString, ByteString, Mat]
  )(implicit random: Random): Flow[ByteString, ByteString, Future[Mat]] =
    Flow
      .fromMaterializer { (_, _) =>
        val localValue = random.nextInt()
        val localValueBytes = intToBytestring(localValue).toArray
        val localValueEvidence = new Blake2b256().hash(Bytes(localValueBytes))
        evidenceFlow(
          Sized.strictUnsafe(localValueEvidence),
          remoteEvidence => {
            if (remoteEvidence.data == localValueEvidence)
              throw new IllegalStateException("Remote party selected the same int value")
            intFlow(
              localValue,
              remoteInt => {
                val remoteValueBytes = intToBytestring(remoteInt).toArray
                val remoteValueHash = new Blake2b256().hash(Bytes(remoteValueBytes))
                if (remoteEvidence.data != remoteValueHash)
                  throw new IllegalStateException("Remote evidence did not match remote value")
                val connectionLeader =
                  if (
                    BigInt(new Blake2b256().hash(Bytes(localValueBytes ++ remoteValueBytes)).toArray) >
                    BigInt(new Blake2b256().hash(Bytes(remoteValueBytes ++ localValueBytes)).toArray)
                  )
                    ConnectionLeaders.Local
                  else ConnectionLeaders.Remote
                f(connectionLeader)
              }
            )
          }
        )
          .mapMaterializedValue(_.flatten)
      }
      .mapMaterializedValue(_.flatten)

  private def intFlow[Mat](localValue: Int, f: Int => Flow[ByteString, ByteString, Mat]) =
    ValueExchanger[Int, Mat](
      localValue,
      co.topl.networking.multiplexer.intToBytestring,
      byteString =>
        Option.when(byteString.length >= 4)(
          (bytestringToInt(byteString), byteString.drop(4))
        ),
      f
    )

  private def evidenceFlow[Mat](localValue: Evidence, f: Evidence => Flow[ByteString, ByteString, Mat]) =
    ValueExchanger[Evidence, Mat](
      localValue,
      evidence => ByteString(evidence.data.toArray),
      byteString =>
        Option.when(byteString.length >= 32)(
          Sized.strictUnsafe[Bytes, Lengths.`32`.type](Bytes(byteString.take(32).toArray)) -> byteString.drop(32)
        ),
      f
    )
}

sealed trait ConnectionLeader

object ConnectionLeaders {
  case object Local extends ConnectionLeader
  case object Remote extends ConnectionLeader
}

class ConnectionLeaderFlow[Mat](f: ConnectionLeader => Flow[ByteString, ByteString, Mat])
    extends GraphStageWithMaterializedValue[FlowShape[ByteString, ByteString], Future[Mat]] {
  private val inlet = Inlet[ByteString]("ConnectionLeaderFlow.In")
  private val outlet = Outlet[ByteString]("ConnectionLeaderFlow.Out")
  val shape: FlowShape[ByteString, ByteString] = FlowShape(inlet, outlet)

  def createLogicAndMaterializedValue(inheritedAttributes: Attributes): (GraphStageLogic, Future[Mat]) = {
    val matPromise: Promise[Mat] = Promise()
    val logic =
      new GraphStageLogic(shape) with StageLogging {

        def setUninitializedHandlers(): Unit = {
          var pullOnInit = false
          var localNumberSent = false
          var remoteNumber = none[Int]
          val localNumber = scala.util.Random.nextInt() // TODO Secure Random

          def moveToInitialized(remoteNumber: Int): Unit = {
            val connectionLeader =
              if (localNumber > remoteNumber) ConnectionLeaders.Local else ConnectionLeaders.Remote
            log.info(s"Initializing sub-handlers with connectionLeader=$connectionLeader")
            setInitializedHandlers(connectionLeader, pullOnInit)
          }

          setHandler(
            inlet,
            new InHandler {
              def onPush(): Unit = {
                val data = grab(inlet)
                if (data.length != 4) throw new IllegalStateException("Received invalid data")
                val rNumber = co.topl.networking.multiplexer.bytestringToInt(data)
                remoteNumber = rNumber.some
                log.info(s"Received remote number $rNumber")
                if (localNumberSent) moveToInitialized(rNumber)
              }

              override def onUpstreamFailure(ex: Throwable): Unit = {
                super.onUpstreamFailure(ex)
                matPromise.failure(ex)
              }
            }
          )
          setHandler(
            outlet,
            new OutHandler {
              def onPull(): Unit =
                if (!localNumberSent) {
                  log.info(s"Sending local number $localNumber")
                  push(outlet, co.topl.networking.multiplexer.intToBytestring(localNumber))
                  remoteNumber match {
                    case Some(n) => moveToInitialized(n)
                    case _       => localNumberSent = true
                  }
                } else {
                  pullOnInit = true
                }

              override def onDownstreamFinish(cause: Throwable): Unit = {
                super.onDownstreamFinish(cause)
                matPromise.failure(cause)
              }
            }
          )
        }

        def setInitializedHandlers(connectionLeader: ConnectionLeader, pullOnInit: Boolean): Unit = {
          val subFlow = f(connectionLeader)
          val subSourceOutlet = new SubSourceOutlet[ByteString]("ConnectionLeaderFlow.Sub.Out")
          val subSinkInlet = new SubSinkInlet[ByteString]("ConnectionLeaderFlow.Sub.In")

          subSourceOutlet.setHandler(
            new OutHandler {
              def onPull(): Unit = pull(inlet)
            }
          )

          subSinkInlet.setHandler(
            new InHandler {
              def onPush(): Unit = push(outlet, subSinkInlet.grab())
            }
          )

          setHandler(
            inlet,
            new InHandler {
              def onPush(): Unit = subSourceOutlet.push(grab(inlet))
            }
          )

          setHandler(
            outlet,
            new OutHandler {
              def onPull(): Unit = subSinkInlet.pull()
            }
          )

          val graph =
            Source.fromGraph(subSourceOutlet.source).viaMat(subFlow)(Keep.right).toMat(subSinkInlet.sink)(Keep.left)
          val mat =
            subFusingMaterializer.materialize(graph)

          matPromise.success(mat)
          if (pullOnInit) subSinkInlet.pull()
        }

        override def preStart(): Unit = {
          super.preStart()
          pull(inlet)
        }

        setUninitializedHandlers()
      }

    logic -> matPromise.future
  }

}
