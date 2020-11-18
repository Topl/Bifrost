package co.topl.attestation.proposition

import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.attestation.Evidence.{EvidenceContent, EvidenceTypePrefix}
import co.topl.attestation.proof.Proof
import co.topl.attestation.secrets.PrivateKeyCurve25519
import co.topl.attestation.{Address, Evidence, EvidenceProducer}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.Curve25519

import scala.util.{Failure, Success}

case class ThresholdPropositionCurve25519 ( threshold: Int, pubKeyProps: Set[PublicKeyPropositionCurve25519])
  extends KnowledgeProposition[PrivateKeyCurve25519] {

  pubKeyProps.foreach(prop => {
    require(prop.pubKeyBytes.length == Curve25519.KeyLength,
      s"Incorrect pubKey length, ${Curve25519.KeyLength} expected, ${prop.pubKeyBytes.length} found")
  })

  val propTypeString: String = ThresholdPropositionCurve25519.typeString
  val propTypePrefix: EvidenceTypePrefix = ThresholdPropositionCurve25519.typePrefix

  def address(implicit networkPrefix: NetworkPrefix): Address = Address.from(this)

}


object ThresholdPropositionCurve25519 {
  // type prefix used for address creation
  val typePrefix: EvidenceTypePrefix = 2: Byte
  val typeString: String = "ThresholdCurve25519"

  val empty = apply("test")

  def apply(str: String): ThresholdPropositionCurve25519 =
    Proposition.fromString(str) match {
      case Success(prop: ThresholdPropositionCurve25519) => prop
      case Failure(ex)                                   => throw ex
    }

  implicit val evProducer: EvidenceProducer[ThresholdPropositionCurve25519] =
    EvidenceProducer.instance[ThresholdPropositionCurve25519] {
      prop: ThresholdPropositionCurve25519 => Evidence(typePrefix, EvidenceContent @@ Blake2b256(prop.bytes))
    }

  // see circe documentation for custom encoder / decoders
  // https://circe.github.io/circe/codecs/custom-codecs.html
  implicit val jsonEncoder: Encoder[ThresholdPropositionCurve25519] = ( prop: ThresholdPropositionCurve25519) => prop.toString.asJson
  implicit val jsonKeyEncoder: KeyEncoder[ThresholdPropositionCurve25519] = ( prop: ThresholdPropositionCurve25519) => prop.toString
  implicit val jsonDecoder: Decoder[ThresholdPropositionCurve25519] = Decoder.decodeString.map(apply)
  implicit val jsonKeyDecoder: KeyDecoder[ThresholdPropositionCurve25519] = ( str: String) => Some(apply(str))
}