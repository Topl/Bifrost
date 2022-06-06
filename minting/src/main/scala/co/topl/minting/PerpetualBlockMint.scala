package co.topl.minting

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.data.OptionT
import cats.effect.kernel.Sync
import cats.implicits._
import cats.{~>, MonadThrow}
import co.topl.algebras.{ClockAlgebra, Store}
import co.topl.catsakka._
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.minting.algebras.{BlockMintAlgebra, PerpetualBlockMintAlgebra}
import co.topl.models.{BlockHeaderV2, BlockV2, Slot, Transaction, TypedIdentifier}
import co.topl.typeclasses.implicits._

import scala.concurrent.Future

object PerpetualBlockMint {

  object InAkkaStream {

    def make[F[_]: Sync: *[_] ~> Future: MonadThrow](
      clock:            ClockAlgebra[F],
      blockMint:        BlockMintAlgebra[F],
      localChain:       LocalChainAlgebra[F],
      mempool:          MempoolAlgebra[F],
      headerStore:      Store[F, TypedIdentifier, BlockHeaderV2],
      fetchTransaction: TypedIdentifier => F[Transaction]
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
                    .mapAsyncF(1)(forSlot)
                    .collect { case Some(newBlock) => newBlock }
                )
            )

          private def forSlot(slot: Slot): F[Option[BlockV2]] =
            for {
              _                     <- clock.delayedUntilSlot(slot)
              canonicalHeadSlotData <- localChain.head
              transactionIds        <- mempool.read(canonicalHeadSlotData.slotId.blockId)
              transactions          <- transactionIds.toList.traverse(fetchTransaction)
              parent <- OptionT(headerStore.get(canonicalHeadSlotData.slotId.blockId)).getOrElseF(
                MonadThrow[F].raiseError(
                  new NoSuchElementException(canonicalHeadSlotData.slotId.blockId.show)
                )
              )
              newBlockOpt <- blockMint.attemptMint(parent, transactions, slot)
            } yield newBlockOpt
        }
      )
  }
}
