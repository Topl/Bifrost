package co.topl.networking.multiplexer

import akka.NotUsed
import akka.stream._
import akka.stream.scaladsl.GraphDSL.Implicits._
import akka.stream.scaladsl.{Flow, GraphDSL, Source}
import akka.stream.stage._
import akka.util.ByteString

import scala.collection.mutable

/**
 * Multiplexes outbound sub-protocol "packets" into a single stream.  Demultiplexes inbound "packets" into multiple
 * sub-protocols.
 *
 * In this case, a "packet" is similar to the TCP notion of a packet, but is meant to be a layer up from
 * the low-level TCP packets.
 *
 * Each inbound "packet" is expected to be in the form of (byte prefix, int length, data).  The packet is read in full
 * before being forwarded onto the sub-protocol matching the packet's byte prefix.
 *
 * Each outbound "packet" is placed into the form of (byte prefix, int length, data).  When a sub-protocol produces data,
 * the multiplexer prepends the sub-protocol's byte prefix and the length of the data.
 */
object Multiplexer {

  def apply(subProtocols: Source[List[SubHandler], _]): Flow[ByteString, ByteString, NotUsed] =
    Flow[ByteString]
      .via(MessageParserFramer())
      .via(
        Flow.fromGraph(GraphDSL.create() { implicit builder =>
          val multiplexer = builder.add(new MultiplexerImpl)
          val subProtocolsB = builder.add(subProtocols)
          subProtocolsB.out ~> multiplexer.subHandlersIn

          FlowShape(multiplexer.dataIn, multiplexer.dataOut)
        })
      )
      .via(MessageSerializerFramer())

}

case class MultiplexerStageShape(
  dataIn:        Inlet[(Byte, ByteString)],
  dataOut:       Outlet[(Byte, ByteString)],
  subHandlersIn: Inlet[List[SubHandler]]
) extends Shape {
  def inlets: Seq[Inlet[_]] = List(dataIn, subHandlersIn)

  def outlets: Seq[Outlet[_]] = List(dataOut)

  def deepCopy(): Shape = this.copy()
}

class MultiplexerImpl extends GraphStage[MultiplexerStageShape] {

  val shape: MultiplexerStageShape =
    MultiplexerStageShape(
      Inlet("MultiplexerData.In"),
      Outlet("MultiplexerData.Out"),
      Inlet("MultiplexerSubHandlers.In")
    )

  def createLogic(inheritedAttributes: Attributes): GraphStageLogic =
    new GraphStageLogic(shape) with StageLogging {
      private var subSinkInlets: Map[Byte, SubSinkInlet[ByteString]] = _
      private var subSourceOutlets: Map[Byte, SubSourceOutlet[ByteString]] = _
      private var localBuffers: Map[Byte, mutable.Queue[ByteString]] = _

      override def preStart(): Unit = {
        super.preStart()
        pull(shape.subHandlersIn)
      }

      private val initializedSubHandlersInHandler =
        new InHandler {

          def onPush(): Unit = {
            updateHandlers(grab(shape.subHandlersIn))
            pull(shape.subHandlersIn)
          }
        }

      private val uninitializedSubHandlersInHandler =
        new InHandler {

          def onPush(): Unit = {
            updateHandlers(grab(shape.subHandlersIn))
            setHandler(shape.dataIn, initializedDataInHandler)
            setHandler(shape.dataOut, initializedOutHandler)
            setHandler(shape.subHandlersIn, initializedSubHandlersInHandler)
            pull(shape.subHandlersIn)
            pull(shape.dataIn)
          }
        }

      private val uninitializedDataInHandler =
        new InHandler {

          def onPush(): Unit =
            failStage(new IllegalStateException("Uninitialized"))
        }

      private val initializedDataInHandler =
        new InHandler {

          def onPush(): Unit = {
            val (sesssionId, data) = grab(shape.dataIn)
            subSourceOutlets.get(sesssionId) match {
              case Some(subSourceOutlet) =>
                if (subSourceOutlet.isAvailable)
                  subSourceOutlet.push(data)
                else
                  localBuffers(sesssionId).enqueue(data)
              case _ =>
                log.warning(s"Discarding message for inactive sessionId={}", sesssionId)
            }
            pull(shape.dataIn)
          }
        }

      private val uninitializedOutHandler =
        new OutHandler {
          def onPull(): Unit = {}
        }

      private val initializedOutHandler =
        new OutHandler {

          def onPull(): Unit =
            localBuffers.find(_._2.nonEmpty) match {
              case Some((id, buffer)) =>
                push(shape.dataOut, id -> buffer.dequeue())
              case _ =>
                pullAllSubHandlers()
            }
        }

      setHandler(shape.dataIn, uninitializedDataInHandler)
      setHandler(shape.dataOut, uninitializedOutHandler)
      setHandler(shape.subHandlersIn, uninitializedSubHandlersInHandler)

      private def pullAllSubHandlers(): Unit =
        subSinkInlets.values.filterNot(_.hasBeenPulled).foreach(_.pull())

      private def updateHandlers(t: List[SubHandler]): Unit = {
        val previousSubSinkInlets = subSinkInlets
        val previousSubSourceOutlets = subSourceOutlets
        val previousLocalBuffers = localBuffers
        if (subSinkInlets == null) subSinkInlets = Map.empty
        if (subSourceOutlets == null) subSourceOutlets = Map.empty
        if (localBuffers == null) localBuffers = Map.empty
        val newIds =
          if (previousLocalBuffers == null) t.map(_.sessionId)
          else t.map(_.sessionId).filterNot(previousLocalBuffers.contains)
        val retainedIds =
          if (previousLocalBuffers == null) Nil else t.map(_.sessionId).filter(previousLocalBuffers.contains)
        val discardedKeys =
          if (previousLocalBuffers == null) Nil else previousLocalBuffers.keys.filterNot(retainedIds.contains).toList
        log.info(
          "Updating handlers from={} to={}",
          Option(previousLocalBuffers).fold(Nil: List[Byte])(_.keys.toList),
          t.map(_.sessionId)
        )
        if (previousSubSinkInlets != null) discardedKeys.foreach(previousSubSinkInlets(_).cancel())
        if (previousSubSourceOutlets != null) discardedKeys.foreach(previousSubSourceOutlets(_).complete())
        // TODO: Drain buffers
        subSinkInlets --= discardedKeys
        subSourceOutlets --= discardedKeys
        localBuffers --= discardedKeys
        t.filter(sh => newIds.contains(sh.sessionId)).foreach(initializeHandler)
        pullAllSubHandlers()
      }

      private def initializeHandler(subHandler: SubHandler): Unit = {
        val sourceOut = new SubSourceOutlet[ByteString]("MultiplexerSubSource")
        val sinkIn = new SubSinkInlet[ByteString]("MultiplexerSubSink")
        val buffer = mutable.Queue.empty[ByteString]
        subSinkInlets += (subHandler.sessionId    -> sinkIn)
        subSourceOutlets += (subHandler.sessionId -> sourceOut)
        localBuffers += (subHandler.sessionId     -> buffer)
        sinkIn.setHandler(
          new InHandler {
            def onPush(): Unit = {
              val data = sinkIn.grab()
              if (isAvailable(shape.dataOut)) push(shape.dataOut, (subHandler.sessionId, data))
              else buffer.enqueue(data)
            }
          }
        )

        sourceOut.setHandler(
          new OutHandler {
            def onPull(): Unit =
              if (buffer.nonEmpty) sourceOut.push(buffer.dequeue())
          }
        )
        val graph = Source.fromGraph(sourceOut.source).via(subHandler.handler).to(sinkIn.sink)
        subFusingMaterializer.materialize(graph)

      }

    }
}
