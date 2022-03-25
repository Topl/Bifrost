package co.topl.networking.typedprotocols.blockchain

import cats.data.Chain
import co.topl.models.TypedIdentifier
import co.topl.networking.typedprotocols.{NotificationProtocol, RequestResponseProtocol}

object BlockchainProtocols {

  object RequestResponseProtocols {

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
    object SlotData extends RequestResponseProtocol[TypedIdentifier, co.topl.models.SlotData]

    /**
     * Request a Header by Block ID
     *
     * This protocol runs a server and client in parallel for each connection.
     */
    object Header extends RequestResponseProtocol[TypedIdentifier, co.topl.models.BlockHeaderV2]

    /**
     * Request a list of Transaction IDs by Block ID
     *
     * This protocol runs a server and client in parallel for each connection.
     */
    object Body extends RequestResponseProtocol[TypedIdentifier, Chain[TypedIdentifier]]

    /**
     * Request a Transaction by Transaction ID
     *
     * This protocol runs a server and client in parallel for each connection.
     */
    object Transaction extends RequestResponseProtocol[TypedIdentifier, co.topl.models.Transaction]

    /**
     * Request the Block ID at some height.  The request also includes the client node's Block ID at the given height.
     *
     * This protocol runs a server and client in parallel for each connection.
     */
    object BlockIdAtHeight
        extends RequestResponseProtocol[(Long, Option[TypedIdentifier]), co.topl.models.TypedIdentifier]
  }

  object NotificationProtocols {

    /**
     * Notifies a client node every time the server adopts a new Block
     *
     * This protocol runs a server and client in parallel for each connection.
     */
    object BlockAdoption extends NotificationProtocol[TypedIdentifier]

    /**
     * Notifies a client node every time the server adds a (new) Transaction to its mempool
     *
     * This protocol runs a server and client in parallel for each connection.
     */
    object Transaction extends NotificationProtocol[TypedIdentifier]
  }
}
