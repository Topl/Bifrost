package co.topl.networking.p2p

import akka.NotUsed
import akka.stream.scaladsl.{Flow, Source}
import akka.stream.stage._
import akka.stream.{Attributes, FlowShape, Inlet, Outlet}
import akka.util.ByteString
import cats.implicits._

/**
 * An Akka-Stream Flow which performs a pre-handshake before materializing the given sub-flow.  This mini-handshake
 * simply establishes a "Connection Leader" with which the given `f` function is applied.  The "Connection Leader" is
 * established by each party sending a random number to the other.  The party with the larger number is deemed to be
 * the "Connection Leader" which can be interpreted as needed when forming the child sub-flow.
 *
 * TODO: Directly sending a random number to the other party allows the other party to delay disclosing their
 * random number.  This allows an adversary to choose to "always" be the leader.  To remedy, the parties should
 * first commit to a number by exchanging a hash of their locally computed random numbers.  Once the commitments are
 * exchanged, the actual numbers can be exchanged and verified.  Next, compare:
 * - BigInt(hash(localNumber.bytes ++ remoteNumber.bytes)) vs. BigInt(hash(remoteNumber.bytes ++ localNumber.bytes))
 *
 * If the hash where the localNumber's bytes came first "wins", then the local party is the "Connection Leader"
 *
 * TODO: Consider a key exchange mechanism
 */
object ConnectionLeaderFlow {

  def apply(f: ConnectionLeader => Flow[ByteString, ByteString, _]): Flow[ByteString, ByteString, NotUsed] =
    Flow.fromGraph(new ConnectionLeaderFlow(f))
}

sealed trait ConnectionLeader

object ConnectionLeaders {
  case object Local extends ConnectionLeader
  case object Remote extends ConnectionLeader
}

class ConnectionLeaderFlow(f: ConnectionLeader => Flow[ByteString, ByteString, _])
    extends GraphStage[FlowShape[ByteString, ByteString]] {
  private val inlet = Inlet[ByteString]("ConnectionLeaderFlow.In")
  private val outlet = Outlet[ByteString]("ConnectionLeaderFlow.Out")
  val shape: FlowShape[ByteString, ByteString] = FlowShape(inlet, outlet)

  def createLogic(inheritedAttributes: Attributes): GraphStageLogic =
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
          Source.fromGraph(subSourceOutlet.source).via(subFlow).to(subSinkInlet.sink)
        subFusingMaterializer.materialize(graph)
        if (pullOnInit) subSinkInlet.pull()
      }

      override def preStart(): Unit = {
        super.preStart()
        pull(inlet)
      }

      setUninitializedHandlers()
    }

}
