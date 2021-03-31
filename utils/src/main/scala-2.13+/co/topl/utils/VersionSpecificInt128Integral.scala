package co.topl.utils

private[utils] trait VersionSpecificInt128Integral {

  /** An implicit providing both Integral and Ordering characteristics to Int128
   */
  implicit object Int128Integral extends Int128IsIntegral with Int128Ordering {
    override def rem(x: Int128, y: Int128): Int128 = x % y

    override def parseString(str: String): Option[Int128] = Option(Int128(str))
  }
}
