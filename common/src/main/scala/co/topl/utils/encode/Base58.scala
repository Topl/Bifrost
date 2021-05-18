package co.topl.utils.encode

import co.topl.utils.codecs.AsBytes.implicits._
import co.topl.utils.codecs.{AsBytes, Infallible}
import co.topl.utils.StringTypes.{Base58String, UTF8String}

/* Forked from https://github.com/ScorexFoundation/scorex-util/tree/master/src/main/scala/scorex/util/encode */

object Base58 extends Encoding[Base58String] {
  val Alphabet: String = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

  private val DecodeTable = Array(
    0, 1, 2, 3, 4, 5, 6, 7, 8, -1, -1, -1, -1, -1, -1, -1, 9, 10, 11, 12, 13, 14, 15, 16, -1, 17, 18, 19, 20, 21, -1,
    22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, -1, -1, -1, -1, -1, -1, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, -1,
    44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57
  )

  private val Base = BigInt(58)

  override def encode[Message: AsBytes](input: Message): Either[EncodingFailure, Base58String] = {
    val inputBytes = input.asBytes

    var bi = BigInt(1, inputBytes)
    val s = new StringBuilder()
    if (bi > 0) {
      while (bi >= Base) {
        val (newBi, mod) = bi /% Base
        s.insert(0, Alphabet.charAt(mod.intValue))
        bi = newBi
      }
      s.insert(0, Alphabet.charAt(bi.intValue))
    }
    // Convert leading zeros too.
    val encodedResult = inputBytes
      .takeWhile(_ == 0)
      .foldLeft(s) { case (ss, _) =>
        ss.insert(0, Alphabet.charAt(0))
      }
      .toString()

    Base58String.validated(encodedResult).leftMap(InvalidString)
  }

  override def decode(input: Base58String): Either[DecodingFailure, Array[Byte]] =
    decodeToBigInteger(input).map { decodedToBigInt =>
      val bytes =
        if (decodedToBigInt == BigInt(0)) Array.emptyByteArray
        else decodedToBigInt.toByteArray

      // We may have got one more byte than we wanted, if the high bit of the next-to-last byte was not zero.
      // This  is because BigIntegers are represented with twos-compliment notation,
      // thus if the high bit of the last  byte happens to be 1 another 8 zero bits will be added to
      // ensure the number parses as positive. Detect that case here and chop it off.
      val stripSignByte = bytes.length > 1 && bytes.head == 0 && bytes(1) < 0
      val stripSignBytePos = if (stripSignByte) 1 else 0
      // Count the leading zeros, if any.
      val leadingZeros = input.value.value.takeWhile(_ == Alphabet.charAt(0)).length

      // Now cut/pad correctly. Java 6 has a convenience for this, but Android
      // can't use it.
      val tmp = new Array[Byte](bytes.length - stripSignBytePos + leadingZeros)
      System.arraycopy(bytes, stripSignBytePos, tmp, leadingZeros, tmp.length - leadingZeros)

      tmp
    }

  def isValidBase58(s: UTF8String): Boolean = !s.value.toCharArray.map(toBase58).contains(-1)

  private def decodeToBigInteger(input: Base58String): Either[InvalidCharacters, BigInt] =
    // Work backwards through the string.
    input.value.value
      .foldRight[Either[InvalidCharacters, (BigInt, BigInt)]](Right(BigInt(0) -> BigInt(1))) {
        case (ch, Right(_)) if toBase58(ch) == -1 => Left(InvalidCharacters())
        case (ch, Right((bi, k)))                 => Right(bi + BigInt(toBase58(ch)) * k -> k * Base)
        case (_, other)                           => other
      }
      .map(_._1)

  private def toBase58(c: Char): Int = {
    val x = c.toInt
    if (x < 49) -1 else if (x <= 122) DecodeTable(x - 49) else -1
  }
}

case class Base58DecodingError()
