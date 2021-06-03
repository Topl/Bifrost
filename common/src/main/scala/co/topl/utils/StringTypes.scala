package co.topl.utils

import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.implicits._
import cats.{Eq, Show}
import co.topl.utils.Extensions.StringOps
import co.topl.utils.encode.{Base16, Base58}
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

import java.util.Locale
import scala.language.implicitConversions

object StringTypes {

  type StringValidationResult[A] = ValidatedNec[StringValidationFailure, A]

  @newtype
  class Utf8String(val value: String)

  object Utf8String {

    def validated(from: String): StringValidationResult[Utf8String] =
      Validated.condNec(from.getValidUTF8Bytes.isDefined, from.coerce, InvalidCharacterSet())

    def unsafe(from: String): Utf8String = from.coerce
  }

  trait UTF8StringInstances {
    implicit val eqUTF8String: Eq[Utf8String] = (a: Utf8String, b: Utf8String) => a.value === b.value

    implicit val showUTF8String: Show[Utf8String] = (value: Utf8String) => value.value
  }

  @newtype
  class Latin1String(val value: String)

  object Latin1String {

    def validated(from: String): StringValidationResult[Latin1String] =
      Validated.condNec(from.getValidLatin1Bytes.isDefined, from.coerce, InvalidCharacterSet())

    def unsafe(from: String): Latin1String = from.coerce
  }

  trait Latin1StringInstances {
    implicit val eqLatin1String: Eq[Latin1String] = (a: Latin1String, b: Latin1String) => a.value === b.value

    implicit val showLatin1String: Show[Latin1String] = (value: Latin1String) => value.value
  }

  @newtype
  class Base58String(val value: Utf8String)

  object Base58String {

    def validated(from: String): StringValidationResult[Base58String] =
      (for {
        validUtf8 <- Utf8String.validated(from).toEither
        isValidBase58 = Base58.isValid(validUtf8)
        validBase58 <- Either.cond(isValidBase58, validUtf8.coerce, NonEmptyChain(InvalidCharacterSet()))
      } yield validBase58).toValidated

    def unsafe(from: String): Base58String = Utf8String.unsafe(from).coerce
  }

  trait Base58StringInstances {

    implicit val eqBase58String: Eq[Base58String] = (a: Base58String, b: Base58String) =>
      a.value.value === b.value.value

    implicit val showBase58String: Show[Base58String] = (value: Base58String) => value.value.value
  }

  @newtype
  class Base16String(val value: Utf8String)

  object Base16String {

    def validated(from: String): StringValidationResult[Base16String] =
      (for {
        validUtf8 <- Utf8String.validated(from.toLowerCase).toEither
        isValidBase16 = Base16.isValid(validUtf8)
        validBase16 <- Either.cond(isValidBase16, validUtf8.coerce, NonEmptyChain(InvalidCharacterSet()))
      } yield validBase16).toValidated

    def validated(from: Array[Char]): StringValidationResult[Base16String] = validated(new String(from))

    def unsafe(from: Array[Char]): Base16String = Utf8String.unsafe(new String(from)).coerce

    def unsafe(from: String): Base16String = Utf8String.unsafe(from).coerce
  }

  trait Base16StringInstances {

    implicit val eqBase16String: Eq[Base16String] = (a: Base16String, b: Base16String) =>
      a.value.value === b.value.value

    implicit val showBase16String: Show[Base16String] = (value: Base16String) => value.value.value
  }

  sealed trait StringValidationFailure
  final case class InvalidCharacterSet() extends StringValidationFailure

  trait StringValidationFailureInstances {

    implicit val showStringValidationFailure: Show[StringValidationFailure] = { case InvalidCharacterSet() =>
      s"Invalid character in string"
    }
  }

  object implicits
      extends UTF8StringInstances
      with Latin1StringInstances
      with Base58StringInstances
      with Base16StringInstances
      with StringValidationFailureInstances

}
