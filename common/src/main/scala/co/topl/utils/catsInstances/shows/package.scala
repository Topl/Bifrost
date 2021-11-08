package co.topl.utils.catsInstances

import cats.Show
import co.topl.codecs.binary.typeclasses.BinaryShow
import co.topl.utils.encode.Base58
import io.circe.Encoder

package object shows extends ShowInstances {
  def fromBinaryShow[T: BinaryShow]: Show[T] = value => Base58.encode(BinaryShow[T].encodeAsBytes(value))

  def fromJsonEncoder[T: Encoder]: Show[T] = value => Encoder[T].apply(value).spaces2
}
