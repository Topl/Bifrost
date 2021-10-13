package co.topl.crypto

sealed trait KeyIndex

object KeyIndexes {

  object Bip32 {
    case class Soft(value: Long) extends KeyIndex
    case class Hardened(value: Long) extends KeyIndex
  }

  object HdKes {
    case class Hour(value: Long) extends KeyIndex
    case class Minute(value: Long) extends KeyIndex
    case class Second(value: Long) extends KeyIndex
  }
}
