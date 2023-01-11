package co.topl.blockchain

import cats.effect.IO
import cats.implicits._
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.models.ModelGenerators._
import co.topl.models.TypedIdentifier
import co.topl.typeclasses.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import fs2._

class MempoolBroadcasterSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("Transactions should be produced in a stream whenever they are added to the mempool") {
    PropF.forAllF { transactionId: TypedIdentifier =>
      withMock {
        for {
          delegate <- mock[MempoolAlgebra[F]].pure[F]
          _ = (delegate.add _).expects(transactionId).once().returning(IO.unit)
          (underTest, txsAdoptionsTopic) <- MempoolBroadcaster.make(delegate)
          i <- txsAdoptionsTopic.subscribeUnbounded
            .concurrently(Stream.eval(underTest.add(transactionId)))
            .head
            .compile
            .lastOrError
          _ = IO(i === transactionId).assert
        } yield ()
      }
    }
  }
}
