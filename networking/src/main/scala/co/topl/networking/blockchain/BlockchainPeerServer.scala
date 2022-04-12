package co.topl.networking.blockchain

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.Applicative
import co.topl.algebras.Store
import co.topl.models.{BlockBodyV2, BlockHeaderV2, Transaction, TypedIdentifier}
import cats.implicits._

/**
 * Serves local blockchain data to a remote peer
 */
trait BlockchainPeerServer[F[_]] {
  def localBlockAdoptions: F[Source[TypedIdentifier, NotUsed]]
  def getLocalHeader(id:      TypedIdentifier): F[Option[BlockHeaderV2]]
  def getLocalBody(id:        TypedIdentifier): F[Option[BlockBodyV2]]
  def getLocalTransaction(id: TypedIdentifier): F[Option[Transaction]]
}

object BlockchainPeerServer {

  object FromStores {

    def make[F[_]: Applicative](
      headerStore:            Store[F, BlockHeaderV2],
      bodyStore:              Store[F, BlockBodyV2],
      transactionStore:       Store[F, Transaction],
      locallyAdoptedBlockIds: Source[TypedIdentifier, NotUsed]
    ): F[BlockchainPeerServer[F]] =
      new BlockchainPeerServer[F] {

        def localBlockAdoptions: F[Source[TypedIdentifier, NotUsed]] =
          locallyAdoptedBlockIds.pure[F]

        def getLocalHeader(id: TypedIdentifier): F[Option[BlockHeaderV2]] = headerStore.get(id)

        def getLocalBody(id: TypedIdentifier): F[Option[BlockBodyV2]] = bodyStore.get(id)

        def getLocalTransaction(id: TypedIdentifier): F[Option[Transaction]] = transactionStore.get(id)
      }
        .pure[F]
  }
}
