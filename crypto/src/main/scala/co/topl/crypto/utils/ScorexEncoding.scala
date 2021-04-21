package co.topl.crypto.utils

import co.topl.crypto.utils.encode.{Base16, BytesEncoder}

/**
  * Trait with bytes to string encoder
  */
trait ScorexEncoding {
  implicit val encoder: BytesEncoder = Base16
}
