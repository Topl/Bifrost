package co.topl.consensus

import cats.MonadError
import cats.data.OptionT
import cats.effect._
import cats.implicits._
import co.topl.algebras.{Cache, Store, UnsafeResource}
import co.topl.crypto.signing.Ed25519VRF
import co.topl.models.{BlockHeaderV2, TypedIdentifier}
import co.topl.typeclasses.implicits._

import scala.concurrent.duration._

trait SlotDataCache[F[_]] {
  def get(blockId: TypedIdentifier): F[SlotData]
}

object SlotDataCache {

  object Eval {

    // TODO: Use Epoch thirds for storage
    def make[F[_]: MonadError[*[_], Throwable]: Clock: Sync](
      blockHeaderLookup:  Store[F, BlockHeaderV2],
      ed25519VRFResource: UnsafeResource[F, Ed25519VRF],
      cache:              F[Cache[F, SlotData]]
    ): F[SlotDataCache[F]] =
      cache.map(cache =>
        (blockId: TypedIdentifier) =>
          cache.cachingF(blockId.show)(ttl = Some(1.day))(
            OptionT(blockHeaderLookup.get(blockId))
              .getOrElseF(new IllegalStateException(blockId.show).raiseError[F, BlockHeaderV2])
              .flatMap(header => ed25519VRFResource.use(implicit ed25519Vrf => SlotData(header).pure[F]))
          )
      )
  }
}
