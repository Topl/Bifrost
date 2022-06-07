package co.topl.grpc

import cats.Applicative
import cats.effect.{Async, IO}
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.grpc.services.{BroadcastTransactionReq, BroadcastTransactionRes}
import co.topl.models.ModelGenerators._
import co.topl.models.Transaction
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.scalatest.MockFactory

class ToplGrpcSpec extends CatsEffectSuite with ScalaCheckEffectSuite with MockFactory {
  type F[A] = IO[A]

  test("A transaction can be broadcast") {
    PropF.forAllF { (transaction: Transaction) =>
      val interpreter = mock[ToplRpc[F]]
      val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

      (interpreter.broadcastTransaction _)
        .expects(transaction)
        .once()
        .returning(Applicative[F].unit)

      Async[F]
        .fromFuture(
          underTest.broadcastTransaction(BroadcastTransactionReq(transaction.immutableBytes)).pure[F]
        )
        .assertEquals(BroadcastTransactionRes())
    }
  }
}
