package co.topl.utils

import java.nio.charset.StandardCharsets

package object serialization {

  /**
   * Character set to use for encoding/decoding `String` types to/from bytes.
   */
  val stringCharacterSet = StandardCharsets.UTF_8
}
