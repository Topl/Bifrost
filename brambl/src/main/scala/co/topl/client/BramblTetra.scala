package co.topl.client

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl._
import cats.data.Chain
import cats.effect.std.Random
import cats.effect.{Async, IO, IOApp}
import cats.implicits._
import co.topl.catsakka.{AkkaCatsRuntime, _}
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.grpc.ToplGrpc
import co.topl.models._
import co.topl.models.utility.HasLength.instances.{bigIntLength, bytesLength}
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration._

object BramblTetra extends IOApp.Simple {

  type F[A] = IO[A]

  override def run: IO[Unit] =
    AkkaCatsRuntime
      .systemResource[F, Nothing](ActorSystem(Behaviors.empty, "Brambl"))
      .use(implicit system =>
        for {
          implicit0(random: Random[F]) <- Random.scalaUtilRandom[F]
          rpcClient1                   <- ToplGrpc.Client.make[F]("localhost", 8090, tls = false)
          logger1                      <- Slf4jLogger.fromName[F]("Brambl@localhost:8090")
          rpcClient2                   <- ToplGrpc.Client.make[F]("localhost", 8091, tls = false)
          logger2                      <- Slf4jLogger.fromName[F]("Brambl@localhost:8091")
          clientLoggerPairs = Array((rpcClient1, logger1), (rpcClient2, logger2))
          source = infiniteTransactionsSource(Box.Id(genesisTransaction.id, 0))
          _ <-
            Async[F].fromFuture(
              source
                .tapAsyncF(1)(transaction =>
                  Random[F]
                    .nextIntBounded(2)
                    .map(clientLoggerPairs.apply)
                    .flatMap { case (client, logger) =>
                      for {
                        _ <- Logger[F](logger).info(show"Broadcasting transaction id=${transaction.id.asTypedBytes}")
                        _ <- client.broadcastTransaction(transaction)
                        _ <- Async[F].sleep(20000.milli)
                      } yield ()
                    }
                )
                .toMat(Sink.seq)(Keep.right)
                .liftTo[F]
            )
        } yield ()
      )

  private def infiniteTransactionsSource(initialBoxId: Box.Id) =
    Source.unfold(initialBoxId)(boxId =>
      Transaction(
        inputs = Chain(
          Transaction.Input(
            boxId = boxId,
            proposition = Propositions.Contextual.HeightLock(1L),
            proof = Proofs.False,
            value = Box.Values.Poly(Sized.maxUnsafe(BigInt(10_000L)))
          )
        ),
        outputs = Chain(
          Transaction.Output(
            address,
            Box.Values.Poly(Sized.maxUnsafe(BigInt(10_000L))),
            minting = false
          )
        ),
        chronology = Transaction.Chronology(System.currentTimeMillis(), 0L, Long.MaxValue),
        data = none
      ).some.map(transaction => Box.Id(transaction.id, 0) -> transaction)
    )

  private val address =
    FullAddress(
      NetworkPrefix(1),
      Propositions.Contextual.HeightLock(1L).spendingAddress,
      StakingAddresses.Operator(VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes.fill(32)(0: Byte)))),
      Proofs.Knowledge.Ed25519(Sized.strictUnsafe(Bytes.fill(64)(0: Byte)))
    )

  private val genesisTransaction =
    Transaction(
      inputs = Chain.empty,
      outputs = Chain(
        Transaction.Output(
          FullAddress(
            NetworkPrefix(1),
            Propositions.Contextual.HeightLock(1L).spendingAddress,
            StakingAddresses.Operator(VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes.fill(32)(0: Byte)))),
            Proofs.Knowledge.Ed25519(Sized.strictUnsafe(Bytes.fill(64)(0: Byte)))
          ),
          Box.Values.Poly(Sized.maxUnsafe(BigInt(10_000L))),
          minting = true
        )
      ),
      chronology = Transaction.Chronology(0L, 0L, Long.MaxValue),
      None
    )

}
