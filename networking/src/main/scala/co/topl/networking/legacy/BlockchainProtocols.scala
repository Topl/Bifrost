package co.topl.networking.legacy

import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.node.models._

/**
 * Defines the various Typed Protocols which are used for the purposes of exchanging blockchain data between
 * two participating nodes.
 */
object BlockchainProtocols {

  /**
   * A simple ping-pong protocol where each "request" contains a number, and the "response" returns the same number.
   * Generally used for measuring latency between peers
   *
   * This protocol runs a server and client in parallel for each connection.
   */
  object KeepAlive extends RequestResponseProtocol[Long, Long] // Long = Cookie = Random number

  /**
   * Request SlotData by Block ID
   *
   * This protocol runs a server and client in parallel for each connection.
   */
  object SlotData extends RequestResponseProtocol[BlockId, SlotData]

  /**
   * Request a Header by Block ID
   *
   * This protocol runs a server and client in parallel for each connection.
   */
  object Header extends RequestResponseProtocol[BlockId, BlockHeader]

  /**
   * Request a list of Transaction IDs by Block ID
   *
   * This protocol runs a server and client in parallel for each connection.
   */
  object Body extends RequestResponseProtocol[BlockId, BlockBody]

  /**
   * Request a Transaction by Transaction ID
   *
   * This protocol runs a server and client in parallel for each connection.
   */
  object Transaction extends RequestResponseProtocol[TransactionId, IoTransaction]

  /**
   * Request the Block ID at some height.  The request also includes the client node's Block ID at the given height.
   *
   * This protocol runs a server and client in parallel for each connection.
   */
  object BlockIdAtHeight extends RequestResponseProtocol[(Long, Option[BlockId]), BlockId]

  /**
   * Request current best tip from remote Node
   */
  object BlockIdAtDepth extends RequestResponseProtocol[Long, BlockId]

  /**
   * Notifies a client node every time the server adopts a new Block
   *
   * This protocol runs a server and client in parallel for each connection.
   */
  object BlockAdoption extends NotificationProtocol[BlockId]

  /**
   * Notifies a client node every time the server adds a (new) Transaction to its mempool
   *
   * This protocol runs a server and client in parallel for each connection.
   */
  object TransactionBroadcasts extends NotificationProtocol[TransactionId]

  /**
   * Requests known hosts from remote peer
   */
  object KnownHosts extends RequestResponseProtocol[CurrentKnownHostsReq, CurrentKnownHostsRes]

  /**
   * Request pong message from remote peer
   */
  object PingPong extends RequestResponseProtocol[PingMessage, PongMessage]

  /**
   * Request address for incoming connections from remote peer
   */
  object RemotePeerServer extends RequestResponseProtocol[Unit, KnownHost]

  /**
   * Notify peer about remote application status
   */
  object ApplicationLevelNotify extends RequestResponseProtocol[Boolean, Unit]
}
