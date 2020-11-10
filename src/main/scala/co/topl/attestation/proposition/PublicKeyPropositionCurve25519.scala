package co.topl.attestation.proposition

import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.attestation.Evidence.{ EvidenceContent, EvidenceTypePrefix }
import co.topl.attestation.secrets.PrivateKeyCurve25519
import co.topl.attestation.{ Address, Evidence, EvidenceProducer }
import io.circe.syntax.EncoderOps
import io.circe.{ Decoder, Encoder, KeyDecoder, KeyEncoder }
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{ Curve25519, PublicKey }

import scala.util.{ Failure, Success }

case class PublicKeyPropositionCurve25519 ( private[proposition] val pubKeyBytes: PublicKey )
  extends KnowledgeProposition[PrivateKeyCurve25519] {

  require(pubKeyBytes.length == Curve25519.KeyLength,
          s"Incorrect pubKey length, ${Curve25519.KeyLength} expected, ${pubKeyBytes.length} found")

  def address (implicit networkPrefix: NetworkPrefix): Address = Address.from(this)

}

object PublicKeyPropositionCurve25519 {
  // type prefix used for address creation
  val typePrefix: EvidenceTypePrefix = 1: Byte

  def apply(str: String): PublicKeyPropositionCurve25519 =
    Proposition.fromString(str) match {
      case Success(pk) => pk
      case Failure(ex) => throw ex
    }

  implicit val propEvidence: EvidenceProducer[PublicKeyPropositionCurve25519] =
    EvidenceProducer.instance[PublicKeyPropositionCurve25519] {
      prop: PublicKeyPropositionCurve25519 => Evidence(typePrefix, EvidenceContent @@ Blake2b256(prop.bytes))
    }

  // see circe documentation for custom encoder / decoders
  // https://circe.github.io/circe/codecs/custom-codecs.html
  implicit val jsonEncoder: Encoder[PublicKeyPropositionCurve25519] = ( prop: PublicKeyPropositionCurve25519) => prop.toString.asJson
  implicit val jsonKeyEncoder: KeyEncoder[PublicKeyPropositionCurve25519] = ( prop: PublicKeyPropositionCurve25519) => prop.toString
  implicit val jsonDecoder: Decoder[PublicKeyPropositionCurve25519] = Decoder.decodeString.map(apply)
  implicit val jsonKeyDecoder: KeyDecoder[PublicKeyPropositionCurve25519] = ( str: String) => Some(apply(str))
}
