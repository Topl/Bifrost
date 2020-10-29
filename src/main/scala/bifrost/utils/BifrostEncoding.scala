package bifrost.utils

/** Trait with bytes to string encoder
  */
trait BifrostEncoding {
  implicit val encoder: BifrostEncoder = BifrostEncoder.default
}
