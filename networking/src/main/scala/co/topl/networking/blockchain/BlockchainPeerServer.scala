package co.topl.networking.blockchain

import akka.NotUsed
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.Source
import cats._
import co.topl.algebras.Store
import co.topl.models.{BlockBody, BlockHeader, SlotData, Transaction, TypedIdentifier}
import co.topl.consensus.models.{BlockHeader => ConsensusBlockHeader} // TODO remove rename, after remove models
import cats.implicits._
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.eventtree.EventSourcedState

/**
 * Serves local blockchain data to a remote peer
 */
trait BlockchainPeerServer[F[_]] {
  def localBlockAdoptions: F[Source[TypedIdentifier, NotUsed]]
  def localTransactionNotifications: F[Source[TypedIdentifier, NotUsed]]
  def getLocalSlotData(id:          TypedIdentifier): F[Option[SlotData]]
  def getLocalHeader(id:            TypedIdentifier): F[Option[ConsensusBlockHeader]]
  def getLocalBody(id:              TypedIdentifier): F[Option[BlockBody]]
  def getLocalTransaction(id:       TypedIdentifier): F[Option[Transaction]]
  def getLocalBlockAtHeight(height: Long): F[Option[TypedIdentifier]]
}

object BlockchainPeerServer {

  object FromStores {

    def make[F[_]: Monad](
      slotDataStore:                Store[F, TypedIdentifier, SlotData],
      headerStore:                  Store[F, TypedIdentifier, ConsensusBlockHeader],
      bodyStore:                    Store[F, TypedIdentifier, BlockBody],
      transactionStore:             Store[F, TypedIdentifier, Transaction],
      blockHeights:                 EventSourcedState[F, Long => F[Option[TypedIdentifier]]],
      localChain:                   LocalChainAlgebra[F],
      locallyAdoptedBlockIds:       F[Source[TypedIdentifier, NotUsed]],
      locallyAdoptedTransactionIds: F[Source[TypedIdentifier, NotUsed]]
    ): F[BlockchainPeerServer[F]] =
      new BlockchainPeerServer[F] {

        def localBlockAdoptions: F[Source[TypedIdentifier, NotUsed]] =
          locallyAdoptedBlockIds.map(_.buffer(1, OverflowStrategy.dropHead))

        def localTransactionNotifications: F[Source[TypedIdentifier, NotUsed]] =
          locallyAdoptedTransactionIds.map(_.buffer(5, OverflowStrategy.dropHead))

        def getLocalSlotData(id: TypedIdentifier): F[Option[SlotData]] = slotDataStore.get(id)

        def getLocalHeader(id: TypedIdentifier): F[Option[ConsensusBlockHeader]] = headerStore.get(id)

        def getLocalBody(id: TypedIdentifier): F[Option[BlockBody]] = bodyStore.get(id)

        def getLocalTransaction(id: TypedIdentifier): F[Option[Transaction]] = transactionStore.get(id)

        def getLocalBlockAtHeight(height: Long): F[Option[TypedIdentifier]] =
          for {
            head          <- localChain.head
            blockHeightsF <- blockHeights.stateAt(head.slotId.blockId)
            blockIdOpt    <- blockHeightsF(height)
          } yield blockIdOpt
      }
        .pure[F]
  }
}
