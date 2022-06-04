package co.topl.client

import akka.NotUsed
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl.Source
import cats.Applicative
import cats.data.Chain
import cats.effect.{Async, IO, IOApp, Resource, Sync}
import co.topl.algebras.ToplRpc
import co.topl.grpc.ToplGrpc
import cats.implicits._
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bigIntLength
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized

object BramblTetra extends IOApp.Simple {

  type F[A] = IO[A]

  override def run: IO[Unit] =
    Resource
      .make(
        Sync[F].delay(ActorSystem(Behaviors.empty, "Brambl"))
      )(system => Sync[F].delay(system.terminate()) >> Async[F].fromFuture(Sync[F].delay(system.whenTerminated)).void)
      .use(implicit system =>
        Resource
          .make(ToplGrpc.Client.make[F]("localhost", 8091))(_ => Applicative[F].unit)
          .use(implicit rpcClient =>
            Transaction(
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
            ).broadcast[F]
          )
      )

  implicit class TransactionOps(transaction: Transaction) {

    def broadcast[F[_]: ToplRpc[*[_], Source[*, NotUsed]]]: F[Unit] =
      implicitly[ToplRpc[F, Source[*, NotUsed]]].broadcastTx(transaction)
  }

}
