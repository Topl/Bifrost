package co.topl.ledger.interpreters

import cats.effect.IO
import cats._
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.eventtree.ParentChildTree
import co.topl.models.ModelGenerators._
import co.topl.models.{BlockBodyV2, TypedBytes, TypedIdentifier}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.{EitherValues, OptionValues}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import co.topl.typeclasses.implicits._

class MempoolSpec
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with EitherValues
    with OptionValues
    with MockFactory {

  behavior of "Mempool"

  type F[A] = IO[A]

  it should "expose a Set of Transaction IDs" in {
    forAll(
      nonEmptyChainOf[(TypedIdentifier, BlockBodyV2)](
        Gen.zip(arbitraryTypedIdentifier.arbitrary, arbitraryBody.arbitrary)
      ),
      arbitraryTypedIdentifier.arbitrary
    ) { case (bodies, newTxId) =>
      val tree = ParentChildTree.FromRef.make[F, TypedIdentifier].unsafeRunSync()
      bodies.map(_._1).sliding2.foreach { case (id1, id2) => tree.associate(id2, id1).unsafeRunSync() }
      val underTest =
        Mempool
          .make[F](
            bodies.head._1.pure[F],
            id => bodies.find(t => Eq[TypedBytes].eqv(t._1, id)).value._2.pure[F],
            tree
          )
          .unsafeRunSync()

      underTest.read(bodies.last._1).unsafeRunSync() shouldBe Set.empty[TypedIdentifier]
      underTest.add(newTxId).unsafeRunSync()
      underTest.read(bodies.last._1).unsafeRunSync() shouldBe Set(newTxId)
      underTest.remove(newTxId).unsafeRunSync()
      underTest.read(bodies.last._1).unsafeRunSync() shouldBe Set.empty[TypedIdentifier]
      underTest.read(bodies.head._1).unsafeRunSync() shouldBe bodies.tail.toList.flatMap(_._2).toSet
    }
  }

  it should "allow transactions to be added externally" in {
    forAll { (currentBlockId: TypedIdentifier, transactionId: TypedIdentifier) =>
      val underTest =
        Mempool
          .make[F](
            currentBlockId.pure[F],
            mockFunction[TypedIdentifier, F[BlockBodyV2]],
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
