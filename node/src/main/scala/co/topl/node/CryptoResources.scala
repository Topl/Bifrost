package co.topl.node

import cats.effect.Async
import cats.implicits._
import co.topl.algebras.UnsafeResource
import co.topl.crypto.signing._
import co.topl.crypto.hash.{Blake2b256, Blake2b512}
import co.topl.interpreters.CatsUnsafeResource

case class CryptoResources[F[_]](
  blake2b256: UnsafeResource[F, Blake2b256],
  blake2b512: UnsafeResource[F, Blake2b512],
  ed25519VRF: UnsafeResource[F, Ed25519VRF],
  kesProduct: UnsafeResource[F, KesProduct],
  ed25519:    UnsafeResource[F, Ed25519]
)

object CryptoResources {

  def make[F[_]: Async]: F[CryptoResources[F]] =
    Async[F]
      .delay(Runtime.getRuntime.availableProcessors())
      .flatMap(maxParallelism =>
        (
          CatsUnsafeResource.make[F, Blake2b256](new Blake2b256, maxParallelism),
          CatsUnsafeResource.make[F, Blake2b512](new Blake2b512, maxParallelism),
          CatsUnsafeResource.make[F, Ed25519VRF](Ed25519VRF.precomputed(), maxParallelism),
          CatsUnsafeResource.make[F, KesProduct](new KesProduct, maxParallelism),
          CatsUnsafeResource.make[F, Ed25519](new Ed25519, maxParallelism)
        ).mapN(CryptoResources[F])
      )
}
