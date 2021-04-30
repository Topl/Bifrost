package co.topl.crypto

import io.estatico.newtype.macros.newtype

import scala.language.implicitConversions

/* Forked from https://github.com/input-output-hk/scrypto */

package object signatures {

  @newtype
  case class PrivateKey(value: Array[Byte])

  @newtype
  case class PublicKey(value: Array[Byte])

  object PublicKey {}

  @newtype
  case class SharedSecret(value: Array[Byte])

  @newtype
  case class Signature(value: Array[Byte])

  object Signature {}

  type MessageToSign = Array[Byte]

}
