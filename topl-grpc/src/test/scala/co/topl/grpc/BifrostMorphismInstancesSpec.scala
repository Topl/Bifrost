package co.topl.grpc

import cats.data.EitherT
import cats.effect.IO
import co.topl.models.ModelGenerators._
import co.topl.models.Proof
import co.topl.{models => bifrostModels}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Arbitrary
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class BifrostMorphismInstancesSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("BlockHeader Morphism") {
    testIsomorphism[bifrostModels.BlockHeaderV2, models.BlockHeader]
  }

  test("BlockBody Morphism") {
    testIsomorphism[bifrostModels.BlockBodyV2, models.BlockBody]
  }

  test("Proof Morphism") {
    // TODO: ModelGenerators makes Curve proofs which fails here
    testIsomorphism[bifrostModels.Proof, models.Proof]
  }

  private def testIsomorphism[A, B](implicit isomorphism: Isomorphism[F, A, B], arbitraryA: Arbitrary[A]) =
    PropF.forAllF { (a: A) =>
      for {
        protoA <- EitherT(a.toF[F, B]).leftMap(new IllegalArgumentException(_)).rethrowT
        _a     <- EitherT(protoA.toF[F, A]).leftMap(new IllegalArgumentException(_)).rethrowT
        _ = assert(a == _a)
      } yield ()
    }
}
