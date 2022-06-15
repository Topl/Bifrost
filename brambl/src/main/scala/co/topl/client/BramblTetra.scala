package co.topl.client

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import cats.data.Chain
import cats.effect.{Async, IO, IOApp, Sync}
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.catsakka.AkkaCatsRuntime
import co.topl.grpc.ToplGrpc
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bigIntLength
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}
import org.typelevel.log4cats.slf4j.Slf4jLogger
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import scala.concurrent.duration._
import co.topl.typeclasses.implicits._

object BramblTetra extends IOApp.Simple {

  type F[A] = IO[A]

  implicit private val logger: F[SelfAwareStructuredLogger[F]] =
    Slf4jLogger.create[F]

  override def run: IO[Unit] =
    AkkaCatsRuntime
      .systemResource[F, Nothing](ActorSystem(Behaviors.empty, "Brambl"))
      .use(implicit system =>
        ToplGrpc.Client
          .make[F]("localhost", 8090, tls = false)
          .flatMap(implicit rpcClient =>
            Slf4jLogger
              .fromName[F]("Brambl@localhost:8090")
              .flatMap(implicit logger => infiniteTransactions(1500.milli))
          )
          .parProduct(
            Async[F].sleep(200.milli) >>
            ToplGrpc.Client
              .make[F]("localhost", 8091, tls = false)
              .flatMap(implicit rpcClient =>
                Slf4jLogger
                  .fromName[F]("Brambl@localhost:8091")
                  .flatMap(implicit logger => infiniteTransactions(1500.milli))
              )
          )
          .void
      )

  private def infiniteTransactions(sleepDuration: FiniteDuration)(implicit rpcClient: ToplRpc[F], logger: Logger[F]) =
    Sync[F]
      .defer(
        for {
          transaction <- Transaction(
            inputs = Chain(
              Transaction.Input(
                boxId = Box.Id(TypedBytes(3: Byte, Bytes.fill(32)(0: Byte)), 0),
                proposition = Propositions.PermanentlyLocked,
                proof = Proofs.False,
                value = Box.Values.Poly(Sized.maxUnsafe(BigInt(10)))
              )
            ),
            outputs = Chain.empty,
            chronology = Transaction.Chronology(System.currentTimeMillis(), 0L, Long.MaxValue),
            data = none
          ).pure[F]
          _ <- Logger[F].info(show"Broadcasting transaction id=${transaction.id.asTypedBytes}")
          _ <- transaction.broadcast[F]
          _ <- Async[F].sleep(sleepDuration)
        } yield ()
      )
      .foreverM
      .void

  implicit class TransactionOps(transaction: Transaction) {
    def broadcast[F[_]: ToplRpc]: F[Unit] = implicitly[ToplRpc[F]].broadcastTransaction(transaction)
  }

}
