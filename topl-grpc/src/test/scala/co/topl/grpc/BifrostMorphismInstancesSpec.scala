package co.topl.grpc

import cats.data.EitherT
import cats.effect.IO
import co.topl.models.ModelGenerators._
import co.topl.{models => bifrostModels}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Arbitrary
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import co.topl.proto.models

class BifrostMorphismInstancesSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("BlockHeader Morphism") {
    testIsomorphism[bifrostModels.BlockHeader, models.BlockHeader]
  }

  test("BlockBody Morphism") {
    testIsomorphism[bifrostModels.BlockBody, models.BlockBody]
  }

  test("Proof Morphism") {
    testIsomorphism[bifrostModels.Proof, models.Proof]
  }

  test("Proposition Morphism") {
    testIsomorphism[bifrostModels.Proposition, models.Proposition]
  }

  test("Box Morphism") {
    testIsomorphism[bifrostModels.Box, models.Box]
  }

  test("Transaction Morphism") {
    testIsomorphism[bifrostModels.Transaction, models.Transaction]
  }

  private def testIsomorphism[A, B](implicit isomorphism: Isomorphism[F, A, B], arbitraryA: Arbitrary[A]) =
    PropF.forAllF { (a: A) =>
      for {
        protoA <- EitherT(a.toF[F, B]).leftMap(new IllegalArgumentException(_)).rethrowT
        _a     <- EitherT(protoA.toF[F, A]).leftMap(new IllegalArgumentException(_)).rethrowT
        _ = assert(a == _a, s"$a did not equal ${_a}")
      } yield ()
    }
}
