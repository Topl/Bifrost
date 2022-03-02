package co.topl.networking

sealed abstract class Party {
  def opposite: Party

  def if_[R](ifA: => R, ifB: => R): R =
    this match {
      case Parties.A => ifA
      case Parties.B => ifB
    }
}

object Parties {

  case object A extends Party {
    def opposite: Party = B
  }

  case object B extends Party {
    def opposite: Party = A
  }
}
