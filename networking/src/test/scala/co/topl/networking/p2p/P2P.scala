package co.topl.networking.p2p

import akka.Done
import akka.actor.ActorSystem
import akka.stream.scaladsl.{Tcp, _}
import akka.util.ByteString
import cats.effect.kernel.Async
import cats.implicits._

import java.net.InetSocketAddress
import scala.concurrent.Future

object P2P {

  def in(host: String, port: Int): Builder1 = new Builder1(host, port)

  class Builder1 private[P2P] (host: String, port: Int) {

    def out(remotes: Source[InetSocketAddress, _]): Builder2 =
      new Builder2(remotes)

    class Builder2 private[P2P] (remotes: Source[InetSocketAddress, _]) {

      def withPeerHandler(flow: Flow[ByteString, ByteString, _]): Builder3 =
        new Builder3(flow)

      class Builder3 private[P2P] (flow: Flow[ByteString, ByteString, _]) {

        def run[F[_]: Async]()(implicit
          system: ActorSystem
        ): F[(Tcp.ServerBinding, Done, Seq[Tcp.OutgoingConnection])] =
          Async[F].fromFuture(
            Async[F].delay {
              val (f1, f2) = inbound.run()
              val f3 = outbound.run()
              import system.dispatcher

              (f1, f2, f3).tupled
            }
          )

        private def inbound(implicit
          system: ActorSystem
        ): RunnableGraph[(Future[Tcp.ServerBinding], Future[Done])] =
          Tcp()(system)
            .bind(host, port)
            .toMat(Sink.foreach { connection =>
              connection.handleWith(flow)
            })(Keep.both)

        private def outbound(implicit system: ActorSystem): RunnableGraph[Future[Seq[Tcp.OutgoingConnection]]] =
          remotes
            .map { r =>
              Tcp()(system)
                .outgoingConnection(r.getHostString, r.getPort)
                .join(flow)
                .run()
            }
            .toMat(Sink.seq)(Keep.right)
            .mapMaterializedValue {
              import system.dispatcher
              _.flatMap(_.sequence)
            }

      }

    }

  }

}

object Demo {

  implicit val system: ActorSystem = ActorSystem("Demo")

  type F[A] = cats.effect.IO[A]

  P2P
    .in(host = "localhost", port = 9004)
    .out(
      remotes = Source(
        List(InetSocketAddress.createUnresolved("1.2.3.4", 9004))
      ).concat(Source.never)
    )
    .withPeerHandler(
      flow = Flow[ByteString].map(ByteString("echo") ++ _)
    )
    .run[F]()
    .unsafeRunSync()(cats.effect.unsafe.implicits.global)
}
