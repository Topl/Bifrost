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


  sealed trait PeerState {
    def networkLevel: Boolean
    def applicationLevel: Boolean

    def activeActor: Boolean = networkLevel || applicationLevel
  }

  object PeerState {
    case object Banned extends PeerState {
      override def networkLevel: Boolean = false

      override def applicationLevel: Boolean = false
    }

    case object Cold extends PeerState {
      override def networkLevel: Boolean = false

      override def applicationLevel: Boolean = false
    }

    case object Warm extends PeerState {
      override def networkLevel: Boolean = true

      override def applicationLevel: Boolean = false
    }

    case object Hot extends PeerState {
      override def networkLevel: Boolean = true

      override def applicationLevel: Boolean = true
    }
  }

}
