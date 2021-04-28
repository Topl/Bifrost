package co.topl.crypto

import io.estatico.newtype.macros.newtype

/* Forked from https://github.com/input-output-hk/scrypto */

package object accumulators {

  @newtype
  case class LeafData(value: Array[Byte])
  @newtype
  case class Side(value: Byte)
  @newtype
  case class ADKey(value: Array[Byte])
  @newtype
  case class ADValue(value: Array[Byte])
  @newtype
  case class ADDigest(value: Array[Byte])
  @newtype
  case class SerializedAdProof(value: Array[Byte])
  @newtype
  case class Balance(value: Byte)

  /** Immutable empty array which can be used in many places to avoid allocations. */
  val EmptyByteArray = Array.empty[Byte]

}
