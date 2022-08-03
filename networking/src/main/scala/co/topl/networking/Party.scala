package co.topl.networking

/**
 * A Party is a participant in a peer-to-peer connection.
 */
sealed abstract class Party {
  def opposite: Party
}

object Parties {

  /**
   * Note: `A` can generally be thought of as the "server"
   */
  case object A extends Party {
    def opposite: Party = B
  }

  /**
   * Note: `B` can generally be thought of as the "client"
   */
  case object B extends Party {
    def opposite: Party = A
  }
}
