package co.topl.crypto

import io.estatico.newtype.macros.newtype

import scala.language.implicitConversions

package object signing {

  @newtype
  case class MessageToSign(value: Array[Byte])

  @newtype
  case class Seed(value: Array[Byte])

  /**
   * The type of the password used alongside a mnemonic to generate a SecretKey
   */
  type Password = String
}
