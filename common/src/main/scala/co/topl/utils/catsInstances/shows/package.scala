package co.topl.utils.catsInstances

import cats.Show
import co.topl.utils.codecs.binary.typeclasses.BinaryShow
import co.topl.utils.encode.Base58

package object shows extends ShowInstances {
  def fromBase58Show[T: BinaryShow]: Show[T] = value => Base58.encode(BinaryShow[T].encodeAsBytes(value))
}
