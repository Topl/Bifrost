package co.topl.utils

import co.topl.utils.StringTypes.StringValidationFailure
import simulacrum.typeclass

import scala.language.implicitConversions

package object encode {

  @typeclass
  trait Encoding[T] {
    def encode[Message](input: Message)(implicit encoder: AsBytes[EncodingFailure]): Either[EncodingFailure, T]

    def decode(input: T): Either[DecodingFailure, Array[Byte]]
  }

  sealed trait EncodingFailure
  case class InvalidString(value: StringValidationFailure) extends EncodingFailure

  sealed trait DecodingFailure
  case class InvalidCharacters() extends DecodingFailure
  case class InvalidDataLength() extends DecodingFailure

  object implicits extends Encoding.ToEncodingOps with Base16EncodingInstance with Base58EncodingInstance
}
