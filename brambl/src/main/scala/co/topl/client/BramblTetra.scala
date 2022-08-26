package co.topl.client

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl._
import cats.data.{Chain, OptionT}
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
          rpcClient1                   <- ToplGrpc.Client.make[F]("localhost", 9084, tls = false)
          logger1                      <- Slf4jLogger.fromName[F]("Brambl@localhost:9084")
//          rpcClient2                   <- ToplGrpc.Client.make[F]("localhost", 8091, tls = false)
//          logger2                      <- Slf4jLogger.fromName[F]("Brambl@localhost:8091")
//          clientLoggerPairs = Array((rpcClient1, logger1), (rpcClient2, logger2))
          clientLoggerPairs = Array((rpcClient1, logger1))
          genesisTransactionId <- OptionT(rpcClient1.blockIdAtHeight(1)).getOrElse(???)
          genesisBody          <- OptionT(rpcClient1.fetchBlockBody(genesisTransactionId)).getOrElse(???)
          genesisTransaction   <- OptionT(rpcClient1.fetchTransaction(genesisBody.head)).getOrElse(???)
          source = infiniteTransactionsSource(
            genesisTransaction,
            Propositions.Contextual.HeightLock(1L),
            findPolyOutputIndex(genesisTransaction)
          )
          _ <-
            Async[F].fromFuture(
              source
                .tapAsyncF(1)(transaction =>
                  Random[F]
                    .nextIntBounded(clientLoggerPairs.length)
                    .map(clientLoggerPairs.apply)
                    .flatMap { case (client, logger) =>
                      for {
                        _ <- Logger[F](logger).info(show"Broadcasting transaction id=${transaction.id.asTypedBytes}")
                        _ <- client.broadcastTransaction(transaction)
                        _ <- Async[F].sleep(10000.milli)
                      } yield ()
                    }
                )
                .toMat(Sink.seq)(Keep.right)
                .liftTo[F]
            )
        } yield ()
      )

  private def findPolyOutputIndex(transaction: Transaction) =
    transaction.outputs.toList
      .indexWhere(_.value match {
        case _: Box.Values.Poly => true
        case _                  => false
      })
      .toShort

  private def infiniteTransactionsSource(transaction: Transaction, proposition: Proposition, polyIndex: Short) =
    Source.unfold((transaction, proposition, polyIndex)) { case (previousTransaction, previousProposition, polyIndex) =>
      Transaction(
        inputs = Chain(
          Transaction.Input(
            boxId = Box.Id(previousTransaction.id, polyIndex),
            proposition = previousProposition,
            proof = Proofs.Contextual.HeightLock(),
            value = Box.Values.Poly(Sized.maxUnsafe(BigInt(10000L)))
          )
        ),
        outputs = Chain(
          Transaction.Output(
            address(Propositions.Contextual.HeightLock(20L)),
            Box.Values.Poly(10000L),
            minting = false
          )
        ),
        schedule = Transaction.Schedule(System.currentTimeMillis(), 0L, Long.MaxValue),
        data = none
      ).some.map(transaction => (transaction, Propositions.Contextual.HeightLock(20L), 0: Short) -> transaction)
    }

  private def address(proposition: Proposition) =
    FullAddress(
      NetworkPrefix(1),
      proposition.spendingAddress,
      StakingAddresses.Operator(VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes.fill(32)(0: Byte)))),
      Proofs.Knowledge.Ed25519(Sized.strictUnsafe(Bytes.fill(64)(0: Byte)))
    )

}
