package co.topl.networking.fsnetwork

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
