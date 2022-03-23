package co.topl.networking.blockchain

import akka.NotUsed
import akka.stream.scaladsl.Source
import co.topl.models.{BlockBodyV2, BlockHeaderV2, TypedIdentifier}

trait BlockchainProtocolServer[F[_]] {
  def localBlockAdoptions: F[Source[TypedIdentifier, NotUsed]]
  def getLocalHeader(id: TypedIdentifier): F[Option[BlockHeaderV2]]
  def getLocalBody(id:   TypedIdentifier): F[Option[BlockBodyV2]]
}
