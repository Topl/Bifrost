package co.topl.networking.p2p

import akka.stream.{Attributes, FlowShape, Inlet, Outlet}
import akka.stream.scaladsl.Flow
import akka.stream.stage.{GraphStage, GraphStageLogic, InHandler, OutHandler}
import akka.util.ByteString

import java.security.SecureRandom

object ConnectionLeaderFlow {

  def apply(f: ConnectionLeader => Flow[ByteString, ByteString, _]): Flow[ByteString, ByteString] =
    ???
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
    new GraphStageLogic(shape) {

      def setUnintializedHandlers(): Unit = {
        val localNumber = scala.util.Random.nextInt() // TODO Secure Random
        setHandler(
          inlet,
          new InHandler {
            def onPush(): Unit = {
              val data = grab(inlet)
              if (data.length != 4) throw new IllegalStateException("Received invalid data")
              val remoteNumber = co.topl.networking.multiplexer.bytestringToInt(data)
              setInitializedHandlers(
                if (localNumber > remoteNumber) ConnectionLeaders.Local else ConnectionLeaders.Remote
              )
            }
          }
        )
      }

      def setInitializedHandlers(connectionLeader: ConnectionLeader): Unit = {
        val subFlow = f(connectionLeader)
        val subSourceOutlet = new SubSourceOutlet[ByteString]("ConnectionLeaderFlow.Sub.Out")
        val subSinkInlet = new SubSinkInlet[ByteString]("ConnectionLeaderFlow.Sub.In")
      }

    }

}
