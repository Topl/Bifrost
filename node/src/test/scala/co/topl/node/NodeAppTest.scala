package co.topl.node

import cats.Applicative
import cats.effect._
import cats.effect.implicits._
import co.topl.algebras.{SynchronizationTraversalSteps, ToplRpc}
import co.topl.grpc.ToplGrpc
import fs2._
import munit._

import scala.concurrent.duration._

class NodeAppTest extends CatsEffectSuite {

  type F[A] = IO[A]

  test("NodeApp should start and produce blocks") {
    val resource =
      for {
        app             <- Sync[F].delay(new NodeApp).toResource
        _               <- Sync[F].delay(app.initialize(Array.empty)).toResource
        _               <- app.run.background
        rpcClient       <- ToplGrpc.Client.make[F]("localhost", 9084, tls = false)
        _               <- awaitNodeReady(rpcClient).toResource
        traversalStream <- rpcClient.synchronizationTraversal().toResource
        _ <- traversalStream
          .collect { case SynchronizationTraversalSteps.Applied(id) => id }
          .evalMap(rpcClient.fetchBlockHeader)
          .map(_.get)
          .takeWhile(_.height < 5)
          .compile
          .toList
          .toResource
      } yield ()
    resource.use_
  }

  private def awaitNodeReady(client: ToplRpc[F, Stream[F, *]]) =
    Stream
      .retry(
        client
          .blockIdAtHeight(1)
          .map(_.get)
          .flatMap(client.fetchBlockHeader)
          .map(_.get.timestamp)
          .flatMap(bigBangTimestamp => Async[F].realTimeInstant.map(bigBangTimestamp - _.toEpochMilli).map(_.milli))
          .flatMap(durationUntilBigBang =>
            Applicative[F].whenA(durationUntilBigBang.toMillis > 0)(Async[F].sleep(durationUntilBigBang))
          ),
        250.milli,
        identity,
        200
      )
      .compile
      .drain
}
