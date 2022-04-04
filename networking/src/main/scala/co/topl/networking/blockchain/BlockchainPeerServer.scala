package co.topl.networking.blockchain

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats._
import co.topl.algebras.Store
import co.topl.models.{BlockBodyV2, BlockHeaderV2, Transaction, TypedIdentifier}
import cats.implicits._
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.eventtree.EventSourcedState

/**
 * Serves local blockchain data to a remote peer
 */
trait BlockchainPeerServer[F[_]] {
  def localBlockAdoptions: F[Source[TypedIdentifier, NotUsed]]
  def getLocalHeader(id:            TypedIdentifier): F[Option[BlockHeaderV2]]
  def getLocalBody(id:              TypedIdentifier): F[Option[BlockBodyV2]]
  def getLocalTransaction(id:       TypedIdentifier): F[Option[Transaction]]
  def getLocalBlockAtHeight(height: Long): F[Option[TypedIdentifier]]
}

object BlockchainPeerServer {

  object FromStores {

    def make[F[_]: Monad](
      headerStore:            Store[F, BlockHeaderV2],
      bodyStore:              Store[F, BlockBodyV2],
      transactionStore:       Store[F, Transaction],
      blockHeights:           EventSourcedState[F, TypedIdentifier, Long => Option[TypedIdentifier]],
      localChain:             LocalChainAlgebra[F],
      locallyAdoptedBlockIds: Source[TypedIdentifier, NotUsed]
    ): F[BlockchainPeerServer[F]] =
      new BlockchainPeerServer[F] {

        def localBlockAdoptions: F[Source[TypedIdentifier, NotUsed]] =
          locallyAdoptedBlockIds.pure[F]

        def getLocalHeader(id: TypedIdentifier): F[Option[BlockHeaderV2]] = headerStore.get(id)

        def getLocalBody(id: TypedIdentifier): F[Option[BlockBodyV2]] = bodyStore.get(id)

        def getLocalTransaction(id: TypedIdentifier): F[Option[Transaction]] = transactionStore.get(id)

        def getLocalBlockAtHeight(height: Long): F[Option[TypedIdentifier]] =
          localChain.head.map(_.slotId.blockId).flatMap(blockHeights.stateAt).ap(height.pure[F])
      }
        .pure[F]
  }
}
