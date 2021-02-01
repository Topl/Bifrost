package co.topl.utils

package object encode {
  /** A helper function for debugging serialization errors */
  def encodeBase16(bytes: Array[Byte]): String= {
    val sb = new StringBuilder
    for (b <- bytes) {
      sb.append(String.format("%02x", Byte.box(b)))
    }
    sb.toString
  }
}
