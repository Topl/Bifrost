package co.topl.node

import cats.effect.Async
import cats.implicits._
import co.topl.algebras.UnsafeResource
import co.topl.catsakka.FToFuture
import co.topl.crypto.hash.{Blake2b256, Blake2b512}
import co.topl.crypto.signing.{Curve25519, Ed25519, Ed25519VRF, ExtendedEd25519, KesProduct}
import co.topl.interpreters.CatsUnsafeResource

case class CryptoResources[F[_]](
  blake2b256:      UnsafeResource[F, Blake2b256],
  blake2b512:      UnsafeResource[F, Blake2b512],
  ed25519VRF:      UnsafeResource[F, Ed25519VRF],
  kesProduct:      UnsafeResource[F, KesProduct],
  curve25519:      UnsafeResource[F, Curve25519],
  ed25519:         UnsafeResource[F, Ed25519],
  extendedEd25519: UnsafeResource[F, ExtendedEd25519]
)

object CryptoResources {

  def make[F[_]: Async: FToFuture]: F[CryptoResources[F]] =
    Async[F]
      .delay(Runtime.getRuntime.availableProcessors())
      .flatMap(maxParallelism =>
        (
          CatsUnsafeResource.make[F, Blake2b256](new Blake2b256, maxParallelism),
          CatsUnsafeResource.make[F, Blake2b512](new Blake2b512, maxParallelism),
          CatsUnsafeResource.make[F, Ed25519VRF](Ed25519VRF.precomputed(), maxParallelism),
          CatsUnsafeResource.make[F, KesProduct](new KesProduct, maxParallelism),
          CatsUnsafeResource.make[F, Curve25519](new Curve25519, maxParallelism),
          CatsUnsafeResource.make[F, Ed25519](new Ed25519, maxParallelism),
          CatsUnsafeResource.make[F, ExtendedEd25519](ExtendedEd25519.precomputed(), maxParallelism)
        ).mapN(CryptoResources[F])
      )
}
