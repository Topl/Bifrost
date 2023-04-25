package co.topl.blockchain

import cats.effect.IO
import cats.implicits._
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models.TransactionId
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.typeclasses.implicits._
import fs2._
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

import scala.concurrent.duration._

class MempoolBroadcasterSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("Transactions should be produced in a stream whenever they are added to the mempool") {
    PropF.forAllF { transactionId: TransactionId =>
      withMock {
        for {
          delegate <- mock[MempoolAlgebra[F]].pure[F]
          _ = (delegate.add _).expects(transactionId).once().returning(IO.unit)
          id <- MempoolBroadcaster
            .make(delegate)
            .flatMap { case (underTest, adoptionsTopic) =>
              adoptionsTopic.subscribeAwaitUnbounded
                .map(_.concurrently(Stream.eval(underTest.add(transactionId))))
            }
            .use(_.head.interruptAfter(3.seconds).compile.lastOrError)
          _ = IO(id === transactionId).assert
        } yield ()
      }
    }
  }
}
