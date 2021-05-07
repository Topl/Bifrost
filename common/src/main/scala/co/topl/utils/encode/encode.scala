package co.topl.utils

import simulacrum.typeclass

import scala.language.implicitConversions

package object encode {

  @typeclass
  trait Encoding[T] {
    def encode[Message: AsBytes](input: Message): Either[EncodingError, T]

    def decode(input: T): Either[DecodingError, Array[Byte]]
  }

  sealed trait EncodingError
  case class StringValidationError(value: StringTypes.StringValidationError) extends EncodingError

  sealed trait DecodingError
  case class InvalidCharactersError() extends DecodingError
  case class InvalidDataLengthError() extends DecodingError

  object implicits extends Encoding.ToEncodingOps with Base16EncodingInstance with Base58EncodingInstance
}
