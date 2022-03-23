package co.topl.minting

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.data.OptionT
import cats.effect.kernel.Sync
import cats.implicits._
import cats.{~>, MonadThrow}
import co.topl.algebras.{ClockAlgebra, MemPoolAlgebra, Store}
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.minting.algebras.{BlockMintAlgebra, PerpetualBlockMintAlgebra}
import co.topl.models.{BlockHeaderV2, BlockV2, Slot}
import co.topl.typeclasses.implicits._

import scala.concurrent.Future

object PerpetualBlockMint {

  object InAkkaStream {

    def make[F[_]: Sync: *[_] ~> Future: MonadThrow](
      clock:       ClockAlgebra[F],
      blockMint:   BlockMintAlgebra[F],
      localChain:  LocalChainAlgebra[F],
      mempool:     MemPoolAlgebra[F],
      headerStore: Store[F, BlockHeaderV2]
    ): F[PerpetualBlockMintAlgebra[F]] =
      Sync[F].delay(
        new PerpetualBlockMintAlgebra[F] {

          def blocks: F[Source[BlockV2, NotUsed]] =
            Sync[F].delay(
              Source
                .future(
                  implicitly[F ~> Future]
                    .apply(clock.globalSlot.map(_.max(0L)).flatMap(s => clock.delayedUntilSlot(s).as(s)))
                )
                .flatMapConcat(initialSlot =>
                  Source
                    .fromIterator(() => Iterator.iterate(initialSlot)(_ + 1))
                    .mapAsync(1)(slot => implicitly[F ~> Future].apply(forSlot(slot)))
                    .collect { case Some(newBlock) => newBlock }
                )
            )

          private def forSlot(slot: Slot): F[Option[BlockV2]] =
            for {
              _                     <- clock.delayedUntilSlot(slot)
              canonicalHeadSlotData <- localChain.head
              transactions          <- mempool.unappliedTransactionsAt(canonicalHeadSlotData.slotId.blockId)
              parent <- OptionT(headerStore.get(canonicalHeadSlotData.slotId.blockId)).getOrElseF(
                MonadThrow[F].raiseError(
                  new NoSuchElementException(canonicalHeadSlotData.slotId.blockId.show)
                )
              )
              newBlockOpt <- blockMint.attemptMint(parent, transactions.toList, slot)
            } yield newBlockOpt
        }
      )
  }
}
