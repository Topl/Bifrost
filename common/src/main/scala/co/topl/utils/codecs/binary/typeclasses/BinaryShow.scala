package co.topl.utils.codecs.binary.typeclasses

import co.topl.crypto.hash.Hash
import co.topl.crypto.hash.digest.Digest
import co.topl.utils.IdiomaticScalaTransition.implicits.toAttemptOps
import co.topl.utils.StringDataTypes.{Base16Data, Base58Data}
import scodec.Encoder
import simulacrum.typeclass

@typeclass
trait BinaryShow[T] {

  def encodeAsBytes(value: T): Array[Byte]

  def encodeAsBase58(value: T): Base58Data = Base58Data.fromData(encodeAsBytes(value))

  def encodeAsBase16(value: T): Base16Data = Base16Data.fromData(encodeAsBytes(value))

  def hash[H, D: Digest](value: T, hashFunc: Hash[H, D]): D = hashFunc.hash(encodeAsBytes(value))
}

object BinaryShow {
  def fromEncoder[T: Encoder]: BinaryShow[T] = Encoder[T].encode(_).getOrThrow().toByteArray
}
