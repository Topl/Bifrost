package co.topl.network

/** Whether the connection is incoming and outgoing */
sealed trait ConnectionDirection {
  def isIncoming: Boolean
  def isOutgoing: Boolean = !isIncoming
}

/** Incoming connection: if a remote address is not in connectionForPeerAddress and unconfirmedConnections it should
  * be an incoming connection
  */
case object Incoming extends ConnectionDirection {
  override val isIncoming: Boolean = true
}

/** Outgoing connection: if a remote address is not in connectionForPeerAddress, and is not yet confirmed(in the
  * unconfirmedConnections set, it should be an outgoing connection)
  */
case object Outgoing extends ConnectionDirection {
  override val isIncoming: Boolean = false
}
