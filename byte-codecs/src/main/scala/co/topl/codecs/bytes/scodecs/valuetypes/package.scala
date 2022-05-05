package co.topl.codecs.bytes.scodecs

import java.nio.charset.{Charset, StandardCharsets}

package object valuetypes extends ValuetypesCodecs {

  object Types {
    type UByte = Int
    type ULong = Long
    type UInt = Long
    type UShort = Int

    type IntString = String
    type ByteString = String
  }

  object Constants {

    /**
     * The number of bits in a byte.
     */
    val byteSize: Int = 8

    val minUIntValue: Long = 0
    val maxUIntValue: Long = 0xffffffffL

    val minUShortValue: Int = 0
    val maxUShortValue: Int = 0xffff

    /**
     * The default character set to use for all `String` encoding/decoding.
     */
    val stringCharacterSet: Charset = StandardCharsets.UTF_8
  }
}
