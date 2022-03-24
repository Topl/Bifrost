package co.topl.networking.multiplexer

import akka.NotUsed
import akka.stream.Attributes
import akka.stream.FlowShape
import akka.stream.Inlet
import akka.stream.Outlet
import akka.stream.scaladsl.Flow
import akka.stream.stage.GraphStage
import akka.stream.stage.GraphStageLogic
import akka.stream.stage.InHandler
import akka.stream.stage.OutHandler
import akka.util.ByteString

import scala.annotation.tailrec

object MessageParserFramer {

  def apply(): Flow[ByteString, (Byte, ByteString), NotUsed] =
    Flow.fromGraph(new MessageParserFramerImpl).mapConcat(identity)
}

private class MessageParserFramerImpl extends GraphStage[FlowShape[ByteString, List[(Byte, ByteString)]]] {

  private val inlet = Inlet[ByteString]("MessageParserFramer.In")

  private val outlet =
    Outlet[List[(Byte, ByteString)]]("MessageParserFramer.Out")

  val shape: FlowShape[ByteString, List[(Byte, ByteString)]] =
    FlowShape(inlet, outlet)

  def createLogic(inheritedAttributes: Attributes): GraphStageLogic =
    new GraphStageLogic(shape) {

      private def processBuffer(
        buffer: ByteString
      ): (List[(Byte, ByteString)], ByteString) = {
        @tailrec
        def inner(
          buffer: ByteString,
          acc:    List[(Byte, ByteString)]
        ): (List[(Byte, ByteString)], ByteString) =
          if (buffer.length < 5) (acc, buffer)
          else {
            val typeByte = buffer.head
            val size = bytestringToInt(buffer.slice(1, 5))
            if (buffer.length < (size + 5)) (acc, buffer)
            else {
              val (b, remaining) = buffer.splitAt(size + 5)
              val data = b.drop(5)
              inner(remaining, acc :+ (typeByte, data))
            }
          }
        inner(buffer, Nil)
      }

      private def processing(buffer: ByteString): InHandler with OutHandler =
        new InHandler with OutHandler {

          def onPush(): Unit = {
            val (out, newBuffer) = processBuffer(buffer ++ grab(inlet))
            if (out.nonEmpty) {
              setHandlers(inlet, outlet, processing(newBuffer))
              push(outlet, out)
            } else {
              setHandlers(inlet, outlet, processing(newBuffer))
              pull(inlet)
            }
          }

          def onPull(): Unit = pull(inlet)
        }

      setHandlers(inlet, outlet, processing(ByteString.empty))
    }
}
