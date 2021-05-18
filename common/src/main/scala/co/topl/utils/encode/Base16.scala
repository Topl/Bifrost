package co.topl.utils.encode

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, ValidatedNec}
import co.topl.utils.StringTypes.{Base16String, UTF8String}
import co.topl.utils.{AsBytes, FromBytes}

/* Forked from https://github.com/ScorexFoundation/scorex-util/tree/master/src/main/scala/scorex/util/encode */

class Base16Encoder extends FromBytes[EncodingFailure, Base16String] {

  override def decode(encoded: Array[Byte]): ValidatedNec[EncodingFailure, Base16String] =
    if (encoded.length == 0)
      // avoid allocation of empty array and new String instance
      Base16String.validated("").leftMap(_.map(InvalidString))
    else {
      val buf = new Array[Char](encoded.length * 2)
      var j = 0
      while (j < encoded.length) {
        val v = encoded(j) & 0xff
        buf(j * 2) = Base16.charIndex(v >>> 4)
        buf(j * 2 + 1) = Base16.charIndex(v & 0x0f)
        j += 1
      }
      Base16String.validated(buf).leftMap(_.map(InvalidString))
    }
}

class Base16Decoder extends AsBytes[DecodingFailure, Base16String] {

  override def encode(input: Base16String): ValidatedNec[DecodingFailure, Array[Byte]] = {
    var isError = false
    var error: Option[DecodingFailure] = None

    val inputValue = input.value.value
    val inputLength = inputValue.length

    val result = Array.ofDim[Byte](inputLength / 2)

    if (inputLength % 2 != 0)
      error = Some(InvalidDataLength())
    else {
      var j = 0
      while (j < inputLength && !isError) {
        val c1 = inputValue(j)
        val c2 = inputValue(j + 1)
        if (c1 > 0 && c1 < 127 && c2 > 0 && c2 < 127) {
          val b1 = Base16.charIndex(c1)
          val b2 = Base16.charIndex(c2)
          if ((b1 | b2) < 0) {
            isError = true
            error = Some(InvalidCharacters())
          } else {
            result(j / 2) = ((b1 << 4) | b2).toByte
          }
        } else {
          isError = true
          error = Some(InvalidCharacters())
        }
        j += 2
      }
    }

    error
      .map(e => Invalid(NonEmptyChain(e)))
      .getOrElse(Valid(result))
  }
}

object Base16 {
  val charIndex: Array[Char] = "0123456789abcdef".toCharArray

  def isValid(s: UTF8String): Boolean =
    s.value.toCharArray
      .map(_.toInt)
      .forall { c =>
        c < 127 && c > 0 && charIndex(c) > 0
      }
}
