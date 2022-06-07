package co.topl.client

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import cats.data.Chain
import cats.effect.{IO, IOApp}
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.catsakka.AkkaCatsRuntime
import co.topl.grpc.ToplGrpc
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bigIntLength
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized

object BramblTetra extends IOApp.Simple {

  type F[A] = IO[A]

  override def run: IO[Unit] =
    AkkaCatsRuntime
      .systemResource[F, Nothing](ActorSystem(Behaviors.empty, "Brambl"))
      .use(implicit system =>
        ToplGrpc.Client
          .make[F]("localhost", 8090, tls = false)
          .flatMap(implicit rpcClient =>
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
    def broadcast[F[_]: ToplRpc]: F[Unit] = implicitly[ToplRpc[F]].broadcastTransaction(transaction)
  }

}
