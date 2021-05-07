package co.topl.utils.encode

import cats.implicits.toBifunctorOps
import co.topl.utils.AsBytes
import co.topl.utils.AsBytes.implicits._
import co.topl.utils.StringTypes.{Base16String, UTF8String}

/* Forked from https://github.com/ScorexFoundation/scorex-util/tree/master/src/main/scala/scorex/util/encode */

object Base16 extends Encoding[Base16String] {

  val Alphabet: String = "0123456789abcdefABCDEF"
  private val hexArray = "0123456789abcdef".toCharArray

  private val hexIndex: Array[Byte] = {
    var index = Array.fill[Byte](128)(0xff.toByte)
    hexArray.zipWithIndex.foreach { case (c, i) =>
      index(c) = i.toByte
    }
    "abcdef".toCharArray.foreach { c =>
      index(c.toUpper) = index(c)
    }
    index
  }

  override def encode[Message: AsBytes](input: Message): Either[EncodingError, Base16String] = {
    val inputBytes = input.asBytes

    if (inputBytes.length == 0)
      // avoid allocation of empty array and new String instance
      Base16String.validated("").leftMap(StringValidationError)
    else {
      val buf = new Array[Char](inputBytes.length * 2)
      var j = 0
      while (j < inputBytes.length) {
        val v = inputBytes(j) & 0xff
        buf(j * 2) = hexArray(v >>> 4)
        buf(j * 2 + 1) = hexArray(v & 0x0f)
        j += 1
      }
      Base16String.validated(buf).leftMap(StringValidationError)
    }
  }

  override def decode(input: Base16String): Either[DecodingError, Array[Byte]] = {
    var isError = false
    var error: Option[DecodingError] = None

    val inputValue = input.value.value
    val inputLength = inputValue.length

    val result = Array.ofDim[Byte](inputLength / 2)

    if (inputLength % 2 != 0)
      error = Some(InvalidDataLengthError())
    else {
      var j = 0
      while (j < inputLength && !isError) {
        val c1 = inputValue(j)
        val c2 = inputValue(j + 1)
        if (c1 > 0 && c1 < 127 && c2 > 0 && c2 < 127) {
          val b1 = hexIndex(c1)
          val b2 = hexIndex(c2)
          if ((b1 | b2) < 0) {
            isError = true
            error = Some(InvalidCharactersError())
          } else {
            result(j / 2) = ((b1 << 4) | b2).toByte
          }
        } else {
          isError = true
          error = Some(InvalidCharactersError())
        }
        j += 2
      }
    }

    error.map(Left(_)).getOrElse(Right(result))
  }

  def isValidBase16(s: UTF8String): Boolean =
    s.value.toCharArray
      .map(_.toInt)
      .forall { c =>
        c < 127 && c > 0 && hexArray(c) > 0
      }
}

trait Base16EncodingInstance {
  implicit val base16Encoding: Encoding[Base16String] = Base16
}
