package co.topl.crypto

import io.estatico.newtype.macros.newtype

package object signing {

  @newtype
  case class MessageToSign(value: Array[Byte])

  @newtype
  case class Seed(value: Array[Byte])
}
