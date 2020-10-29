package co.topl.attestation.proposition

import co.topl.attestation.Evidence.{EvidenceContent, EvidenceTypePrefix}
import co.topl.attestation.secrets.PrivateKeyCurve25519
import co.topl.attestation.{Evidence, KnowledgeProposition}
import co.topl.utils.serialization.BifrostSerializer
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{Curve25519, PublicKey}
import scorex.util.encode.Base58

import scala.util.{Failure, Success, Try}

case class PublicKeyCurve25519Proposition (private[proposition] val pubKeyBytes: PublicKey) extends KnowledgeProposition[PrivateKeyCurve25519] {

  require(pubKeyBytes.length == Curve25519.KeyLength,
    s"Incorrect pubKey length, ${Curve25519.KeyLength} expected, ${pubKeyBytes.length} found")

  override type M = PublicKeyCurve25519Proposition

  override val typePrefix: EvidenceTypePrefix = PublicKeyCurve25519Proposition.typePrefix

  override def serializer: BifrostSerializer[PublicKeyCurve25519Proposition] = PublicKeyCurve25519PropositionSerializer

  override def toString: String = Base58.encode(bytes)

  override def equals(obj: Any): Boolean = obj match {
    case p: PublicKeyCurve25519Proposition => p.pubKeyBytes sameElements pubKeyBytes
    case _                                 => false
  }

  override def hashCode(): Int = (BigInt(Blake2b256(pubKeyBytes)) % Int.MaxValue).toInt

//  def verify(message: Array[Byte], signature: Signature): Boolean = Curve25519.verify(signature, message, pubKeyBytes)

}



object PublicKeyCurve25519Proposition {
  // type prefix used for address creation
  val typePrefix: EvidenceTypePrefix = 1: Byte

  implicit val propEvidence: Evidence[PublicKeyCurve25519Proposition] =
    Evidence.instance[PublicKeyCurve25519Proposition] {
      prop: PublicKeyCurve25519Proposition => EvidenceContent @@ Blake2b256(prop.bytes).repr
    }

  def apply(pkStr: String): PublicKeyCurve25519Proposition =
    fromString(pkStr) match {
      case Success(pk) => pk
      case Failure(ex) => throw ex
    }

  def fromString(pkStr: String): Try[PublicKeyCurve25519Proposition] =
    Base58.decode(pkStr).map { pubKeyBytes =>
          PublicKeyCurve25519Proposition(PublicKey @@ pubKeyBytes)
    }

  // see circe documentation for custom encoder / decoders
  // https://circe.github.io/circe/codecs/custom-codecs.html
  implicit val jsonEncoder: Encoder[PublicKeyCurve25519Proposition] =
    (prop: PublicKeyCurve25519Proposition) => prop.toString.asJson

  implicit val jsonKeyEncoder: KeyEncoder[PublicKeyCurve25519Proposition] =
    (prop: PublicKeyCurve25519Proposition) => prop.toString

  implicit val jsonDecoder: Decoder[PublicKeyCurve25519Proposition] =
    Decoder.decodeString.emapTry(fromString)

  implicit val jsonKeyDecoder: KeyDecoder[PublicKeyCurve25519Proposition] = fromString(_: String).toOption
}
