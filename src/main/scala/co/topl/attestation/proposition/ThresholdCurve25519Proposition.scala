package co.topl.attestation.proposition

import co.topl.attestation.Evidence.{EvidenceContent, EvidenceTypePrefix}
import co.topl.attestation.proof.ThresholdSignatureCurve25519
import co.topl.attestation.secrets.PrivateKeyCurve25519
import co.topl.attestation.{Evidence, KnowledgeProposition}
import co.topl.utils.serialization.BifrostSerializer
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{Curve25519, Signature}
import scorex.util.encode.Base58

import scala.util.{Failure, Success, Try}

case class ThresholdCurve25519Proposition (threshold: Int, pubKeyProps: Set[PublicKeyCurve25519Proposition]) extends KnowledgeProposition[PrivateKeyCurve25519] {

  pubKeyProps.foreach(prop => {
    require(prop.pubKeyBytes.length == Curve25519.KeyLength,
            s"Incorrect pubKey length, ${Curve25519.KeyLength} expected, ${prop.pubKeyBytes.length} found")
  })

  override type M = ThresholdCurve25519Proposition

  override val typePrefix: EvidenceTypePrefix = ThresholdCurve25519Proposition.typePrefix

  override def serializer: BifrostSerializer[ThresholdCurve25519Proposition] = ThresholdCurve25519PropositionSerializer

  override def toString: String = Base58.encode(bytes)

  // TODO: only works for m == 1
  def verify(message: Array[Byte], signature: ThresholdSignatureCurve25519): Boolean = {

  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case p: ThresholdCurve25519Proposition => p.threshold == threshold && p.pubKeyProps == pubKeyProps
    case _                                 => false
  }

  override def hashCode(): Int = (BigInt(Blake2b256(serializer.toBytes(this))) % Int.MaxValue).toInt
}


object ThresholdCurve25519Proposition {
  // type prefix used for address creation
  val typePrefix: EvidenceTypePrefix = 2: Byte

  implicit val propEvidence: Evidence[ThresholdCurve25519Proposition] =
    Evidence.instance[ThresholdCurve25519Proposition] {
      prop: ThresholdCurve25519Proposition =>
        EvidenceContent @@ Blake2b256(prop.pubKeyProps.foldLeft(Array[Byte]())(_ ++ _.pubKeyBytes)).repr
    }

  def apply(propStr: String): ThresholdCurve25519Proposition =
    fromString(propStr) match {
      case Success(prop) => prop
      case Failure(ex) => throw ex
    }

  def fromString(propStr: String): Try[ThresholdCurve25519Proposition] =
    Base58.decode(propStr).flatMap(ThresholdCurve25519PropositionSerializer.parseBytes)

  // see circe documentation for custom encoder / decoders
  // https://circe.github.io/circe/codecs/custom-codecs.html
  implicit val jsonEncoder: Encoder[ThresholdCurve25519Proposition] =
  (prop: ThresholdCurve25519Proposition) => prop.toString.asJson

  implicit val jsonKeyEncoder: KeyEncoder[ThresholdCurve25519Proposition] =
    (prop: ThresholdCurve25519Proposition) => prop.toString

  implicit val jsonDecoder: Decoder[ThresholdCurve25519Proposition] =
    Decoder.decodeString.emapTry(fromString)

  implicit val jsonKeyDecoder: KeyDecoder[ThresholdCurve25519Proposition] = fromString(_: String).toOption
}