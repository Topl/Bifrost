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

case class ThresholdCurve25519Proposition (threshold: Int, pubKeyProps: Set[PublicKeyCurve25519Proposition])
  extends KnowledgeProposition[PrivateKeyCurve25519] {

  pubKeyProps.foreach(prop => {
    require(prop.pubKeyBytes.length == Curve25519.KeyLength,
            s"Incorrect pubKey length, ${Curve25519.KeyLength} expected, ${prop.pubKeyBytes.length} found")
  })

  def address(implicit networkPrefix: NetworkPrefix): Address = Address.from(this)

}


object ThresholdCurve25519Proposition {
  // type prefix used for address creation
  val typePrefix: EvidenceTypePrefix = 2: Byte

  def apply(str: String): ThresholdCurve25519Proposition =
    Proof.fromString(str) match {
      case Success(prop) => prop
      case Failure(ex) => throw ex
    }

  implicit val propEvidence: EvidenceProducer[ThresholdCurve25519Proposition] =
    EvidenceProducer.instance[ThresholdCurve25519Proposition] {
      prop: ThresholdCurve25519Proposition => Evidence(typePrefix, EvidenceContent @@ Blake2b256(prop.bytes))
    }

  // see circe documentation for custom encoder / decoders
  // https://circe.github.io/circe/codecs/custom-codecs.html
  implicit val jsonEncoder: Encoder[ThresholdCurve25519Proposition] = (prop: ThresholdCurve25519Proposition) => prop.toString.asJson
  implicit val jsonKeyEncoder: KeyEncoder[ThresholdCurve25519Proposition] = (prop: ThresholdCurve25519Proposition) => prop.toString
  implicit val jsonDecoder: Decoder[ThresholdCurve25519Proposition] = Decoder.decodeString.map(apply)
  implicit val jsonKeyDecoder: KeyDecoder[ThresholdCurve25519Proposition] = (str: String) => Some(apply(str))
}