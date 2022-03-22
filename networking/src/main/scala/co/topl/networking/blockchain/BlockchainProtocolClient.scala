package co.topl.networking.blockchain

import akka.NotUsed
import akka.stream.scaladsl.Source
import co.topl.models.{BlockHeaderV2, TypedIdentifier}

trait BlockchainProtocolClient[F[_]] {
  def remotePeerAdoptions: F[Source[TypedIdentifier, NotUsed]]
  def getRemoteHeader(id: TypedIdentifier): F[Option[BlockHeaderV2]]
}
