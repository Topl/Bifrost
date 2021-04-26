package co.topl.crypto

import io.estatico.newtype.macros.newtype

/* Forked from https://github.com/input-output-hk/scrypto */

package object signatures {

  @newtype
  case class PrivateKey(toBytes: Array[Byte])

  @newtype
  case class PublicKey(toBytes: Array[Byte])

  @newtype
  case class SharedSecret(toBytes: Array[Byte])

  @newtype
  case class Signature(toBytes: Array[Byte])

  type MessageToSign = Array[Byte]

}
