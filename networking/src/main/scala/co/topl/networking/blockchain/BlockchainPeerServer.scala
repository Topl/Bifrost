package co.topl.networking.blockchain

import akka.NotUsed
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.Source
import cats._
import co.topl.algebras.Store
import co.topl.models.{BlockBodyV2, BlockHeaderV2, Transaction, TypedIdentifier}
import cats.implicits._
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.eventtree.EventSourcedState
import org.typelevel.log4cats.Logger

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

    def make[F[_]: Monad: Logger](
      headerStore:            Store[F, TypedIdentifier, BlockHeaderV2],
      bodyStore:              Store[F, TypedIdentifier, BlockBodyV2],
      transactionStore:       Store[F, TypedIdentifier, Transaction],
      blockHeights:           EventSourcedState[F, TypedIdentifier, Long => F[Option[TypedIdentifier]]],
      localChain:             LocalChainAlgebra[F],
      locallyAdoptedBlockIds: Source[TypedIdentifier, NotUsed]
    ): F[BlockchainPeerServer[F]] =
      new BlockchainPeerServer[F] {

        def localBlockAdoptions: F[Source[TypedIdentifier, NotUsed]] =
          locallyAdoptedBlockIds.buffer(1, OverflowStrategy.dropHead).pure[F]

        def getLocalHeader(id: TypedIdentifier): F[Option[BlockHeaderV2]] = headerStore.get(id)

        def getLocalBody(id: TypedIdentifier): F[Option[BlockBodyV2]] = bodyStore.get(id)

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
