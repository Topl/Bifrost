package co.topl.networking.blockchain

import cats.effect.Async
import co.topl.algebras.Store
import co.topl.catsakka._
import co.topl.models.{SlotData, Transaction, TypedIdentifier}
import co.topl.consensus.models.{BlockHeader => ConsensusBlockHeader} // TODO remove rename, after remove models
import co.topl.node.models.{BlockBody => NodeBlockBody} // TODO remove rename, after remove models
import cats.implicits._
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.eventtree.EventSourcedState
import fs2._

/**
 * Serves local blockchain data to a remote peer
 */
trait BlockchainPeerServer[F[_]] {
  def localBlockAdoptions: F[Stream[F, TypedIdentifier]]
  def localTransactionNotifications: F[Stream[F, TypedIdentifier]]
  def getLocalSlotData(id:          TypedIdentifier): F[Option[SlotData]]
  def getLocalHeader(id:            TypedIdentifier): F[Option[ConsensusBlockHeader]]
  def getLocalBody(id:              TypedIdentifier): F[Option[NodeBlockBody]]
  def getLocalTransaction(id:       TypedIdentifier): F[Option[Transaction]]
  def getLocalBlockAtHeight(height: Long): F[Option[TypedIdentifier]]
}

object BlockchainPeerServer {

  object FromStores {

    def make[F[_]: Async](
      slotDataStore:                Store[F, TypedIdentifier, SlotData],
      headerStore:                  Store[F, TypedIdentifier, ConsensusBlockHeader],
      bodyStore:                    Store[F, TypedIdentifier, NodeBlockBody],
      transactionStore:             Store[F, TypedIdentifier, Transaction],
      blockHeights:                 EventSourcedState[F, Long => F[Option[TypedIdentifier]], TypedIdentifier],
      localChain:                   LocalChainAlgebra[F],
      locallyAdoptedBlockIds:       F[Stream[F, TypedIdentifier]],
      locallyAdoptedTransactionIds: F[Stream[F, TypedIdentifier]]
    ): F[BlockchainPeerServer[F]] =
      new BlockchainPeerServer[F] {

        def localBlockAdoptions: F[Stream[F, TypedIdentifier]] =
          locallyAdoptedBlockIds.map(_.dropOldest(1))

        def localTransactionNotifications: F[Stream[F, TypedIdentifier]] =
          locallyAdoptedTransactionIds.map(_.dropOldest(5))

        def getLocalSlotData(id: TypedIdentifier): F[Option[SlotData]] = slotDataStore.get(id)

        def getLocalHeader(id: TypedIdentifier): F[Option[ConsensusBlockHeader]] = headerStore.get(id)

        def getLocalBody(id: TypedIdentifier): F[Option[NodeBlockBody]] = bodyStore.get(id)

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
