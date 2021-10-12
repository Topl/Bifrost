package co.topl.consensus

import cats.MonadError
import cats.data.OptionT
import cats.effect._
import cats.implicits._
import co.topl.algebras.BlockHeaderLookup
import co.topl.crypto.signatures.Ed25519VRF
import co.topl.models.{BlockHeaderV2, TypedIdentifier}
import co.topl.typeclasses.implicits._
import scalacache.cachingF
import scalacache.caffeine.CaffeineCache

import scala.concurrent.duration._

trait SlotDataCache[F[_]] {
  def get(blockId: TypedIdentifier): F[SlotData]
}

object SlotDataCache {

  object Eval {

    def make[F[_]: MonadError[*[_], Throwable]: Clock: Sync](
      blockHeaderLookup: BlockHeaderLookup[F]
    ): F[SlotDataCache[F]] =
      Ref
        .of[F, Ed25519VRF](Ed25519VRF.precomputed())
        .flatMap(ref =>
          CaffeineCache[F, SlotData].map { implicit cache => (blockId: TypedIdentifier) =>
            // TODO: Use Epoch thirds for storage
            cachingF(blockId)(ttl = Some(1.day))(
              OptionT(blockHeaderLookup.getBlockHeader(blockId))
                .getOrElseF(new IllegalStateException(blockId.show).raiseError[F, BlockHeaderV2])
                .flatMap(header => ref.modify(implicit ed25519Vrf => ed25519Vrf -> SlotData(header)))
          )
          }
        )
  }
}
