package co.topl.ledger.interpreters

import cats.implicits._
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.algebras.Store
import co.topl.eventtree.ParentChildTree
import co.topl.models.{BlockBodyV2, TypedIdentifier}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import co.topl.models.ModelGenerators._
import org.scalamock.scalatest.MockFactory

class MempoolSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks with EitherValues with MockFactory {

  behavior of "Mempool"

  type F[A] = IO[A]

  it should "expose a Set of Transaction IDs" in {
    assert(false)
  }

  it should "allow transactions to be added externally" in {
    forAll { (currentBlockId: TypedIdentifier, transactionId: TypedIdentifier) =>
      val underTest =
        Mempool
          .make[F](
            currentBlockId.pure[F],
            mock[Store[F, TypedIdentifier, BlockBodyV2]],
            mock[Store[F, TypedIdentifier, BlockBodyV2]],
            mock[ParentChildTree[F, TypedIdentifier]]
          )
          .unsafeRunSync()

      underTest.add(transactionId).unsafeRunSync()
    }
  }

  it should "evict transactions after they have expired" in {
    assert(false)
  }

}
