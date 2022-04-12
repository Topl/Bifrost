package co.topl.networking.blockchain

import akka.NotUsed
import akka.stream.scaladsl.Source
import co.topl.models.{BlockBodyV2, BlockHeaderV2, Transaction, TypedIdentifier}

/**
 * A client which can be used by a local node to retrieve blockchain data from a remote peer
 */
trait BlockchainPeerClient[F[_]] {
  def remotePeerAdoptions: F[Source[TypedIdentifier, NotUsed]]
  def getRemoteHeader(id:      TypedIdentifier): F[Option[BlockHeaderV2]]
  def getRemoteBody(id:        TypedIdentifier): F[Option[BlockBodyV2]]
  def getRemoteTransaction(id: TypedIdentifier): F[Option[Transaction]]
}
