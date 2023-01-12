package co.topl.blockchain

import cats.data.NonEmptyChain
import co.topl.models.TypedIdentifier
import co.topl.proto.models.FullBlockBody

package object network {

  type HostId = String //IP address? IP address could be changed and bad for identify good peer
  type HostReputationValue = Long //will be more complex, to get high reputation host shall fulfill different criteria
  type HostReputationUpdate = (HostId, HostReputationValue)
  type HostReputations = Map[HostId, HostReputationValue]


  case class HeadersCandidate(source: HostId, headers: NonEmptyChain[TypedIdentifier])
  //we need to know source of block, because block itself could be wrong
  case class NotVerifiedBlock(source: HostId, id: TypedIdentifier, block: FullBlockBody)

  case class BlockGettingError(id: TypedIdentifier, error: String)

  type ErrorOrBlock = Either[BlockGettingError, NotVerifiedBlock] //if block is verified no need to verify it twice.


  object PeerState extends Enumeration {
    type PeerState = Value
    val Banned, Cold, Warm, Hot = Value
  }

}
