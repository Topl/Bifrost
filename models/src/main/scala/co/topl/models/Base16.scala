package co.topl.models

import cats.implicits._

/* Forked from https://github.com/ScorexFoundation/scorex-util/tree/master/src/main/scala/scorex/util/encode */

object Base16 {

  sealed trait Base16DecodingFailure
  case class InvalidCharacter(c: String) extends Base16DecodingFailure
  case class NonEvenLength() extends Base16DecodingFailure

  private val characters: Array[Char] = "0123456789abcdef".toCharArray

  def encode(bytes: Array[Byte]): String =
    if (bytes.length == 0)
      // avoid allocation of empty array and new String instance
      ""
    else {
      val buf = new Array[Char](bytes.length * 2)
      var j = 0
      while (j < bytes.length) {
        val v = bytes(j) & 0xff
        buf(j * 2) = characters(v >>> 4)
        buf(j * 2 + 1) = characters(v & 0x0f)
        j += 1
      }
      new String(buf)
    }

  def decode(input: String): Either[Base16DecodingFailure, Array[Byte]] =
    if (input.length % 2 != 0)
      Left(NonEvenLength())
    else {
      input.grouped(2).foldLeft(Array[Byte]().asRight[Base16DecodingFailure]) {
        case (Right(bytes), chars) =>
          val c1 = chars(0)
          val c2 = chars(1)
          if (c1 > 0 && c1 < 127 && c2 > 0 && c2 < 127) {
            val b1 = base16Indices(c1)
            val b2 = base16Indices(c2)
            if ((b1 | b2) < 0) {
              Left(InvalidCharacter(chars))
            } else {
              Right(bytes :+ ((b1 << 4) | b2).toByte)
            }
          } else {
            Left(InvalidCharacter(chars))
          }
        case (err, _) => err
      }
    }

  def isValid(s: String): Boolean =
    s.toCharArray
      .forall { c =>
        characters.contains(c)
      } && (s.length % 2 == 0)

  private val base16Indices: Array[Byte] = {
    val indices = Array.fill[Byte](128)(0xff.toByte)
    characters.zipWithIndex.foreach { case (c, i) =>
      indices(c) = i.toByte
    }
    "abcdef".toCharArray.foreach { c =>
      indices(c.toUpper) = indices(c)
    }
    indices
  }
}
