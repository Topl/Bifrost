package co.topl.minting.interpreters

import cats.effect.IO
import cats.effect.IO.asyncForIO
import cats.effect.implicits.effectResourceOps
import co.topl.crypto.signing.Ed25519VRF
import co.topl.interpreters.CatsUnsafeResource
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility._
import com.google.protobuf.ByteString
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import scodec.bits._

class VrfCalculatorSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  // test the cache implementation is not possible in the current implementation,
  // Time to live is not used on testing for both vrfProofs and rhos caches
  val vrfCacheTtl = 0L

  test("proofForSlot: fixed input") {
    for {
      ed25519Resource <- CatsUnsafeResource.make(new Ed25519VRF, 1).toResource
      vrfCalculator <- VrfCalculator.make[F](
        skVrf = ByteString.copyFrom(Array.fill[Byte](32)(0)),
        ed25519Resource,
        vrfCacheTtl
      )

      slot = 10L
      eta = Sized.strictUnsafe(ByteString.copyFrom(Array.fill[Byte](32)(0))): Eta

      expectedProof =
        ByteString.copyFrom(
          hex"bc31a2fb46995ffbe4b316176407f57378e2f3d7fee57d228a811194361d8e7040c9d15575d7a2e75506ffe1a47d772168b071a99d2e85511730e9c21397a1cea0e7fa4bd161e6d5185a94a665dd190d".toArray
        )

      _ <- vrfCalculator.proofForSlot(slot, eta).assertEquals(expectedProof).toResource
    } yield ()
  }

  test("rhoForSlot: fixed input") {
    val resource = for {
      ed25519Resource <- CatsUnsafeResource.make(new Ed25519VRF, 1).toResource
      vrfCalculator <- VrfCalculator.make[F](
        skVrf = ByteString.copyFrom(Array.fill[Byte](32)(0)),
        ed25519Resource,
        vrfCacheTtl
      )

      slot = 10L
      eta = Sized.strictUnsafe(ByteString.copyFrom(Array.fill[Byte](32)(0))): Eta

      expectedRho = Rho(
        Sized.strictUnsafe(
          data =
            hex"c30d2304d5d76e7cee8cc0eb66493528cc9e5a9cc03449bc8ed3dab192ba1e8edb3567b4ffc63526c69a6d05a73b57879529ccf8dd22e596080257843748d569"
        )
      )

      _ <- vrfCalculator.rhoForSlot(slot, eta).assertEquals(expectedRho).toResource
    } yield ()
    resource.use_
  }

}
