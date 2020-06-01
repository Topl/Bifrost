package bifrost.utils

import bifrost.utils.encode.{Base16, BytesEncoder}

/**
  * Trait with bytes to string encoder
  */
trait BifrostEncoding {
  implicit val encoder: BytesEncoder = Base16
}
