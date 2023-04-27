package co.topl.networking.blockchain

import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.SlotData
import co.topl.networking.typedprotocols.NotificationProtocol
import co.topl.networking.typedprotocols.RequestResponseProtocol
import co.topl.node.models.BlockBody

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
  object CurrentTip extends RequestResponseProtocol[Unit, SlotData]

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

}
