package co.topl

package object config {

  // scalastyle:off magic.number
  def toMultiplier(percent: Double): Double =
    percent.max(0).min(100) / 100
  // scalastyle:on magic.number
}
