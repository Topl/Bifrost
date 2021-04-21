package co.topl.utils.serialization

object ZigZagEncoder {

  /**
    * Encode a ZigZag-encoded 32-bit value.  ZigZag encodes signed integers
    * into values that can be efficiently encoded with varint.  (Otherwise,
    * negative values must be sign-extended to 64 bits to be varint encoded,
    * thus always taking 10 bytes on the wire.)
    *
    * @see [[https://developers.google.com/protocol-buffers/docs/encoding#types]]
    *
    * @param n signed Int
    * @return unsigned Int stored in a signed Int
    */
  def encodeZigZagInt(n: Int): Int = {
    // Note:  the right-shift must be arithmetic
    // source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedOutputStream.java#L934
    (n << 1) ^ (n >> 31)
  }

  /**
    * Decode a signed value previously ZigZag-encoded with [[encodeZigZagInt]]
    *
    * @see [[https://developers.google.com/protocol-buffers/docs/encoding#types]]
    * @param n unsigned Int previously encoded with [[encodeZigZagInt]]
    * @return signed Int
    */
  def decodeZigZagInt(n: Int): Int = {
    // source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedInputStream.java#L553
    (n >>> 1) ^ -(n & 1)
  }

  /**
    * Encode a ZigZag-encoded 64-bit value.  ZigZag encodes signed integers
    * into values that can be efficiently encoded with varint.  (Otherwise,
    * negative values must be sign-extended to 64 bits to be varint encoded,
    * thus always taking 10 bytes on the wire.)
    *
    * @see [[https://developers.google.com/protocol-buffers/docs/encoding#types]]
    * @param n signed Long
    * @return unsigned Long stored in a signed Long
    */
  def encodeZigZagLong(n: Long): Long = {
    // source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedOutputStream.java#L949
    // Note:  the right-shift must be arithmetic
    (n << 1) ^ (n >> 63)
  }

  /**
    * Decode a signed value previously ZigZag-encoded with [[encodeZigZagLong]]
    *
    * @see [[https://developers.google.com/protocol-buffers/docs/encoding#types]]
    * @param n unsigned Long previously encoded with [[encodeZigZagLong]]
    * @return signed Long
    */
  def decodeZigZagLong(n: Long): Long = {
    // source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedInputStream.java#L566
    (n >>> 1) ^ -(n & 1)
  }

}
