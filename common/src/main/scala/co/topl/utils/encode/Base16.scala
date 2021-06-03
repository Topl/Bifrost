package co.topl.utils.encode

import co.topl.utils.StringTypes.{Base16String, UTF8String}

/* Forked from https://github.com/ScorexFoundation/scorex-util/tree/master/src/main/scala/scorex/util/encode */

object Base16 {

  private val characters: Array[Char] = "0123456789abcdef".toCharArray

  def encode(bytes: Array[Byte]): Base16String =
    if (bytes.length == 0)
      // avoid allocation of empty array and new String instance
      Base16String.unsafe("")
    else {
      val buf = new Array[Char](bytes.length * 2)
      var j = 0
      while (j < bytes.length) {
        val v = bytes(j) & 0xff
        buf(j * 2) = characters(v >>> 4)
        buf(j * 2 + 1) = characters(v & 0x0f)
        j += 1
      }
      Base16String.unsafe(buf)
    }

  def decode(input: Base16String): Array[Byte] = {
    val inputValue = input.value.value
    val inputLength = inputValue.length

    val result = Array.ofDim[Byte](inputLength / 2)

    if (inputLength % 2 != 0)
      throw new Error("Invalid Base 16 string: Input length is not even")
    else {
      var j = 0
      while (j < inputLength) {
        val c1 = inputValue(j)
        val c2 = inputValue(j + 1)
        if (c1 > 0 && c1 < 127 && c2 > 0 && c2 < 127) {
          val b1 = base16Indices(c1)
          val b2 = base16Indices(c2)
          if ((b1 | b2) < 0) {
            throw new Error("Invalid character in Base 16 string!")
          } else {
            result(j / 2) = ((b1 << 4) | b2).toByte
          }
        } else {
          throw new Error("Invalid character in Base 16 string!")
        }
        j += 2
      }
    }

    result
  }

  def unsafeDecode(input: String): Array[Byte] = decode(Base16String.unsafe(input))

  def isValid(s: UTF8String): Boolean =
    s.value.toCharArray
      .forall { c =>
        characters.contains(c)
      } && (s.value.length % 2 == 0)

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
