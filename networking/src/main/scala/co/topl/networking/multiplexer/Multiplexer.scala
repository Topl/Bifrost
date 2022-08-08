package co.topl.networking.multiplexer

import akka.stream._
import akka.stream.scaladsl.GraphDSL.Implicits._
import akka.stream.scaladsl.{Flow, GraphDSL, Merge, Partition}
import akka.util.ByteString
import cats.data.NonEmptyChain

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

  private val buffer = Flow[ByteString].buffer(1, OverflowStrategy.backpressure)

  def apply[Client](subProtocols: NonEmptyChain[SubHandler], client: => Client): Flow[ByteString, ByteString, Client] =
    Flow[ByteString]
      .via(MessageParserFramer())
      .via(
        Flow.fromGraph(GraphDSL.create() { implicit builder =>
          val subProtocolsList = subProtocols.toChain.toList
          val subPortMapping: Map[Byte, Int] = subProtocolsList.map(_.sessionId).zipWithIndex.toMap
          val partition = builder.add(
            new Partition[(Byte, ByteString)](
              subProtocolsList.size,
              { case (sessionId, _) => subPortMapping(sessionId) },
              eagerCancel = true
            )
          )

          val merge =
            builder.add(
              Merge[(Byte, ByteString)](subProtocolsList.size, eagerComplete = true)
            )
          subProtocolsList.foreach { case SubHandler(sessionId, sink, source) =>
            val port = subPortMapping(sessionId)
            val hFlow = builder.add(Flow.fromSinkAndSource(buffer.to(sink), source))
            val stripSessionByte = builder.add(Flow[(Byte, ByteString)].map(_._2))
            val appendSessionByte = builder.add(Flow[ByteString].map((sessionId, _)))
            partition.out(port) ~> stripSessionByte ~> hFlow ~> appendSessionByte ~> merge.in(port)
          }
          FlowShape(partition.in, merge.out)
        })
      )
      .via(MessageSerializerFramer())
      .mapMaterializedValue(_ => client)

}
