package co.topl.networking.blockchain

import akka.NotUsed
import akka.stream.scaladsl.Source
import co.topl.models.{BlockBodyV2, BlockHeaderV2, TypedIdentifier}

/**
 * Serves local blockchain data to a remote peer
 */
trait BlockchainPeerServer[F[_]] {
  def localBlockAdoptions: F[Source[TypedIdentifier, NotUsed]]
  def getLocalHeader(id: TypedIdentifier): F[Option[BlockHeaderV2]]
  def getLocalBody(id:   TypedIdentifier): F[Option[BlockBodyV2]]
}
