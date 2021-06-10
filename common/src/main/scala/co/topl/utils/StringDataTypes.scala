package co.topl.utils

import cats.data.ValidatedNec
import cats.implicits._
import cats.{Eq, Show}
import co.topl.utils.Extensions.StringOps
import co.topl.utils.encode.{Base16, Base58}
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

import java.nio.charset.StandardCharsets
import scala.language.implicitConversions

object StringDataTypes {

  type DataValidationResult[A] = ValidatedNec[DataValidationFailure, A]

  @newtype
  class Latin1Data(val value: Array[Byte])

  object Latin1Data {

    def fromData(bytes: Array[Byte]): Latin1Data = bytes.coerce

    def validated(from: String): DataValidationResult[Latin1Data] =
      from.getValidLatin1Bytes.toValidNec(InvalidCharacterSet("Invalid Latin-1 data.")).map(_.coerce)

    def unsafe(from: String): Latin1Data =
      validated(from).valueOr(err => throw new IllegalArgumentException(s"Invalid Latin-1 string: $err"))
  }

  trait Latin1DataInstances {
    implicit val eqLatin1Data: Eq[Latin1Data] = (a: Latin1Data, b: Latin1Data) => a.value sameElements b.value

    implicit val showLatin1Data: Show[Latin1Data] =
      latin1Data => new String(latin1Data.value, StandardCharsets.ISO_8859_1)
  }

  @newtype
  class Base58Data(val value: Array[Byte])

  object Base58Data {

    def fromData(bytes: Array[Byte]): Base58Data = bytes.coerce

    def validated(from: String): DataValidationResult[Base58Data] =
      Base58.decode(from).map(_.coerce).leftMap(err => InvalidCharacterSet(err.toString)).toValidatedNec

    def unsafe(from: String): Base58Data =
      validated(from).valueOr(err => throw new IllegalArgumentException(s"Invalid Base-58 string: $err"))
  }

  trait Base58DataInstances {
    implicit val eqBase58String: Eq[Base58Data] = (a: Base58Data, b: Base58Data) => a.value sameElements b.value

    implicit val showBase58String: Show[Base58Data] = data => Base58.encode(data.value)
  }

  @newtype
  class Base16Data(val value: Array[Byte])

  object Base16Data {

    def fromData(bytes: Array[Byte]): Base16Data = bytes.coerce

    def validated(from: String): DataValidationResult[Base16Data] =
      Base16.decode(from).map(_.coerce).leftMap(err => InvalidCharacterSet(err.toString)).toValidatedNec

    def unsafe(from: String): Base16Data =
      validated(from).valueOr(err => throw new IllegalArgumentException(s"Invalid Base-16 string: $err"))
  }

  trait Base16DataInstances {

    implicit val eqBase16String: Eq[Base16Data] = (a: Base16Data, b: Base16Data) => a.value sameElements b.value

    implicit val showBase16String: Show[Base16Data] = base16Data => Base16.encode(base16Data.value)
  }

  sealed trait DataValidationFailure
  case class InvalidCharacterSet(message: String) extends DataValidationFailure

  object implicits extends Latin1DataInstances with Base16DataInstances with Base58DataInstances
}
