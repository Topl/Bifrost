package co.topl.crypto

import co.topl.models.utility.Bip32Index

sealed trait KeyIndex

object KeyIndexes {

  sealed abstract class Bip32(val value: Long)

  object Bip32 {
    case class Soft(override val value: Long) extends Bip32(value)
    case class Hardened(override val value: Long) extends Bip32(value + Bip32Index.hardenedOffset)
  }

  object HdKes {
    case class Hour(value: Long) extends KeyIndex
    case class Minute(value: Long) extends KeyIndex
    case class Second(value: Long) extends KeyIndex
  }
}
