package co.topl.utils

private[utils] trait VersionSpecificInt128Integral {

  /** An implicit providing both Integral and Ordering characteristics to Int128
   */
  implicit object Int128Integral extends Int128IsIntegral with Int128Ordering
}
