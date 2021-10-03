package co.topl.crypto

import io.estatico.newtype.macros.newtype

import scala.language.implicitConversions

/* Forked from https://github.com/input-output-hk/scrypto */

package object signing {
  @newtype
  case class MessageToSign(value: Array[Byte])

  @newtype
  case class Seed(value: Array[Byte])
}
