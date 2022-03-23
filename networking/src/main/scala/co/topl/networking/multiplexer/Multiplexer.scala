package co.topl.networking.multiplexer

import akka.stream._
import akka.stream.scaladsl.GraphDSL.Implicits._
import akka.stream.scaladsl.{Flow, GraphDSL, Merge, Partition, Sink, Source}
import akka.util.ByteString

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

  def apply[Client](subProtocols: List[SubHandler], client: => Client): Flow[ByteString, ByteString, Client] =
    Flow[ByteString]
      .via(MessageParserFramer())
      .via(
        Flow.fromGraph(GraphDSL.create() { implicit builder =>
          val subs: List[(Byte, Sink[ByteString, _], Source[ByteString, _])] =
            subProtocols
              .map(sp =>
                (
                  sp.sessionId,
                  Flow[ByteString]
                    .map((sp.sessionId, _))
                    .via(MessageSerializerFramer())
                    .to(sp.subscriber),
                  sp.producer
                )
              )
          val subPortMapping: Map[Byte, Int] = subs.map(_._1).zipWithIndex.toMap
          val partition = builder.add(
            new Partition[(Byte, ByteString)](
              subs.size,
              { case (typeByte, _) =>
                subPortMapping(typeByte)
              },
              eagerCancel = true
            )
          )

          val merge =
            builder.add(Merge[ByteString](subs.size, eagerComplete = true))
          subs.foreach { case (typeByte, sink, source) =>
            val port = subPortMapping(typeByte)
            val hSink = builder.add(sink)
            val hSource = builder.add(source)
            val strip = builder.add(Flow[(Byte, ByteString)].map(_._2))
            partition.out(port) ~> strip ~> hSink
            hSource ~> merge.in(port)
          }
          FlowShape(partition.in, merge.out)
        })
      )
      .mapMaterializedValue(_ => client)

}
