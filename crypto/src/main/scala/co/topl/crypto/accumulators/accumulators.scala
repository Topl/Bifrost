package co.topl.crypto

import io.estatico.newtype.macros.newtype

/* Forked from https://github.com/input-output-hk/scrypto */

package object accumulators {

  @newtype
  case class LeafData(toBytes: Array[Byte])
  @newtype
  case class Side(toBytes: Byte)
  @newtype
  case class ADKey(toBytes: Array[Byte])
  @newtype
  case class ADValue(toBytes: Array[Byte])
  @newtype
  case class ADDigest(toBytes: Array[Byte])
  @newtype
  case class SerializedAdProof(toBytes: Array[Byte])
  @newtype
  case class Balance(toBytes: Byte)

  /** Immutable empty array which can be used in many places to avoid allocations. */
  val EmptyByteArray = Array.empty[Byte]
}
