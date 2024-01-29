package co.topl.blockchain

import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.brambl.utils.CatsUnsafeResource
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.hash.Blake2b512
import co.topl.crypto.signing._

case class CryptoResources[F[_]](
  blake2b256: Resource[F, Blake2b256],
  blake2b512: Resource[F, Blake2b512],
  ed25519VRF: Resource[F, Ed25519VRF],
  kesProduct: Resource[F, KesProduct],
  ed25519:    Resource[F, Ed25519]
)

object CryptoResources {

  def make[F[_]: Async]: F[CryptoResources[F]] =
    Async[F]
      // Limit the number of each resource to the number of available processors,
      // but with a minimum of 4 to avoid scarcity
      .delay((Runtime.getRuntime.availableProcessors() - 1).min(1))
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
