package co.topl.consensus

import cats.MonadError
import cats.data.OptionT
import cats.effect._
import cats.implicits._
import co.topl.algebras.{Store, UnsafeResource}
import co.topl.crypto.signing.Ed25519VRF
import co.topl.models.{BlockHeaderV2, Bytes, SlotData, TypedIdentifier}
import co.topl.typeclasses.implicits._
import scalacache.cachingF
import scalacache.caffeine.CaffeineCache

import scala.concurrent.duration._

trait SlotDataCache[F[_]] {
  def get(blockId: TypedIdentifier): F[SlotData]
}

object SlotDataCache {

  object Eval {

    // TODO: Use Epoch thirds for storage
    def make[F[_]: MonadError[*[_], Throwable]: Clock: Sync](
      blockHeaderLookup:  Store[F, TypedIdentifier, BlockHeaderV2],
      ed25519VRFResource: UnsafeResource[F, Ed25519VRF]
    ): F[SlotDataCache[F]] =
      CaffeineCache[F, Bytes, SlotData].map(implicit cache =>
        (blockId: TypedIdentifier) =>
          cachingF(blockId.allBytes)(ttl = Some(1.day))(
            OptionT(blockHeaderLookup.get(blockId))
              .getOrElseF(new IllegalStateException(blockId.show).raiseError[F, BlockHeaderV2])
              .flatMap(header => ed25519VRFResource.use(implicit ed25519Vrf => header.slotData.pure[F]))
          )
      )
  }
}
