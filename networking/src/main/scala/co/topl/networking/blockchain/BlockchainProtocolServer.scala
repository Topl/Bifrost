package co.topl.networking.blockchain

import akka.NotUsed
import akka.stream.scaladsl.Source
import co.topl.models.{BlockHeaderV2, TypedIdentifier}

trait BlockchainProtocolServer[F[_]] {
  def localBlockAdoptions: F[Source[TypedIdentifier, NotUsed]]
  def getLocalHeader(id: TypedIdentifier): F[Option[BlockHeaderV2]]
}
