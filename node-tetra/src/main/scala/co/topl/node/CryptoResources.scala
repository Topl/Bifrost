package co.topl.node

import akka.actor.typed.ActorSystem
import akka.util.Timeout
import cats.effect.Async
import cats.implicits._
import co.topl.algebras.UnsafeResource
import co.topl.catsakka.FToFuture
import co.topl.crypto.hash.{Blake2b256, Blake2b512}
import co.topl.crypto.signing.{Curve25519, Ed25519, Ed25519VRF, ExtendedEd25519, KesProduct}
import co.topl.interpreters.ActorPoolUnsafeResource

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

  def make[F[_]: Async: FToFuture](implicit system: ActorSystem[_], timeout: Timeout): F[CryptoResources[F]] =
    (
      ActorPoolUnsafeResource.Eval.make[F, Blake2b256](new Blake2b256, _ => ()),
      ActorPoolUnsafeResource.Eval.make[F, Blake2b512](new Blake2b512, _ => ()),
      ActorPoolUnsafeResource.Eval.make[F, Ed25519VRF](Ed25519VRF.precomputed(), _ => ()),
      ActorPoolUnsafeResource.Eval.make[F, KesProduct](new KesProduct, _ => ()),
      ActorPoolUnsafeResource.Eval.make[F, Curve25519](new Curve25519, _ => ()),
      ActorPoolUnsafeResource.Eval.make[F, Ed25519](new Ed25519, _ => ()),
      ActorPoolUnsafeResource.Eval
        .make[F, ExtendedEd25519](ExtendedEd25519.precomputed(), _ => ())
    ).mapN(CryptoResources[F])
}
