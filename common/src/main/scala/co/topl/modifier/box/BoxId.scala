package co.topl.modifier.box

import cats.implicits._
import co.topl.attestation.Evidence
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.hash.digest.Digest32
import co.topl.crypto.hash.implicits._
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.StringDataTypes.implicits._
import co.topl.codecs._
import co.topl.utils.encode.Base58
import com.google.common.primitives.{Ints, Longs}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

case class BoxId(hash: Digest32) {

  override def hashCode: Int = Ints.fromByteArray(hash.value)

  override def equals(obj: Any): Boolean = obj match {
    case obj: BoxId => obj.hash === hash
    case _          => false
  }

  override def toString: String = Base58.encode(hash.value)
}

object BoxId {

  val size: Int = Digest32.size // boxId is a 32 byte identifier

  def apply[T](box: Box[T]): BoxId = idFromEviNonce(box.evidence, box.nonce)

  def idFromEviNonce(evidence: Evidence, nonce: Box.Nonce): BoxId =
    BoxId(blake2b256.hash(evidence.persistedBytes ++ Longs.toByteArray(nonce)))
}
