package co.topl.codecs.json.attestation

import cats.implicits._
import co.topl.attestation.AddressCodec.implicits._
import co.topl.attestation._
import co.topl.codecs.binary._
import co.topl.codecs.json.valuetypes._
import co.topl.codecs.json.{
  deriveDecoderFromScodec,
  deriveEncoderFromScodec,
  deriveKeyDecoderFromScodec,
  deriveKeyEncoderFromScodec
}
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.encode.Base58
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

trait AttestationCodecs extends co.topl.codecs.json.attestation.keyManagement.KeyManagementCodecs {

  private val evidenceTypeName = "Evidence"
  private val proofTypeName = "Proof"
  private val publicKeyPropositionCurve25519TypeName = "Public Key Proposition Curve 25519"
  private val thresholdPropositionCurve25519TypeName = "Threshold Proposition Curve 25519"
  private val publicKeyPropositionEd25519TypeName = "Public Key Proposition ED 25519"
  private val signatureCurve25519TypeName = "Signature Curve 25519"
  private val thresholdSignatureCurve25519TypeName = "Threshold Signature Curve 25519"
  private val signatureEd25519TypeName = "Signature ED 25519"
  private val propositionTypeName = "Proposition"

  implicit val addressJsonEncoder: Encoder[Address] = addr => addr.encodeAsBase58.asJson

  implicit val addressJsonKeyEncoder: KeyEncoder[Address] = addr => Base58.encode(addr.encodeAsBytes)

  implicit def addressJsonDecoder(implicit networkPrefix: NetworkPrefix): Decoder[Address] =
    Decoder[Base58Data].emap(_.decodeAddress.toEither.leftMap(_.toString))

  implicit def addressJsonKeyDecoder(implicit networkPrefix: NetworkPrefix): KeyDecoder[Address] =
    json => Base58Data.validated(json).toOption.flatMap(_.decodeAddress.toOption)

  implicit val evidenceJsonEncoder: Encoder[Evidence] = deriveEncoderFromScodec[Evidence](evidenceTypeName)

  implicit val evidenceJsonKeyEncoder: KeyEncoder[Evidence] = deriveKeyEncoderFromScodec[Evidence](evidenceTypeName)

  implicit val evidenceJsonDecoder: Decoder[Evidence] = deriveDecoderFromScodec[Evidence](evidenceTypeName)

  implicit val evidenceJsonKeyDecoder: KeyDecoder[Evidence] = deriveKeyDecoderFromScodec[Evidence](evidenceTypeName)

  implicit def proofJsonEncoder[P <: Proposition]: Encoder[Proof[P]] =
    deriveEncoderFromScodec[Proof[P]](proofTypeName)

  implicit val proofJsonDecoder: Decoder[Proof[_ <: Proposition]] =
    deriveDecoderFromScodec(proofTypeName)(proofCodec.asDecoder)

  implicit val publicKeyPropositionCurve25519JsonEncoder: Encoder[PublicKeyPropositionCurve25519] =
    deriveEncoderFromScodec(publicKeyPropositionCurve25519TypeName)(
      publicKeyPropositionCurve25519Codec.asEncoder
    )

  implicit val publicKeyPropositionCurve25519JsonKeyEncoder: KeyEncoder[PublicKeyPropositionCurve25519] =
    deriveKeyEncoderFromScodec(publicKeyPropositionCurve25519TypeName)(
      publicKeyPropositionCurve25519Codec.asEncoder
    )

  implicit val publicKeyPropositionCurve25519JsonDecoder: Decoder[PublicKeyPropositionCurve25519] =
    deriveDecoderFromScodec(publicKeyPropositionCurve25519TypeName)(
      publicKeyPropositionCurve25519Codec.asDecoder
    )

  implicit val publciKeyPropositionCurve25519JsonKeyDecoder: KeyDecoder[PublicKeyPropositionCurve25519] =
    deriveKeyDecoderFromScodec(publicKeyPropositionCurve25519TypeName)(
      publicKeyPropositionCurve25519Codec.asDecoder
    )

  implicit val thresholdPropositionCurve25519JsonEncoder: Encoder[ThresholdPropositionCurve25519] =
    deriveEncoderFromScodec(thresholdPropositionCurve25519TypeName)(thresholdPropositionCurve25519Codec.asEncoder)

  implicit val thresholdPropositionCurve25519JsonKeyEncoder: KeyEncoder[ThresholdPropositionCurve25519] =
    deriveKeyEncoderFromScodec(thresholdPropositionCurve25519TypeName)(
      thresholdPropositionCurve25519Codec.asEncoder
    )

  implicit val thresholdPropositionCurve25519JsonDecoder: Decoder[ThresholdPropositionCurve25519] =
    deriveDecoderFromScodec(thresholdPropositionCurve25519TypeName)(thresholdPropositionCurve25519Codec.asDecoder)

  implicit val thresholdPropositionCurve25519JsonKeyDecoder: KeyDecoder[ThresholdPropositionCurve25519] =
    deriveKeyDecoderFromScodec(thresholdPropositionCurve25519TypeName)(
      thresholdPropositionCurve25519Codec.asDecoder
    )

  implicit val publicKeyPropositionEd25519JsonEncoder: Encoder[PublicKeyPropositionEd25519] =
    deriveEncoderFromScodec(publicKeyPropositionEd25519TypeName)(publicKeyPropositionEd25519Codec.asEncoder)

  implicit val publicKeyPropositionEd25519JsonKeyEncoder: KeyEncoder[PublicKeyPropositionEd25519] =
    deriveKeyEncoderFromScodec(publicKeyPropositionEd25519TypeName)(publicKeyPropositionEd25519Codec.asEncoder)

  implicit val publicKeyPropositionEd25519JsonDecoder: Decoder[PublicKeyPropositionEd25519] =
    deriveDecoderFromScodec(publicKeyPropositionEd25519TypeName)(publicKeyPropositionEd25519Codec.asDecoder)

  implicit val publicKeyPropositionEd25519JsonKeyDecoder: KeyDecoder[PublicKeyPropositionEd25519] =
    deriveKeyDecoderFromScodec(publicKeyPropositionEd25519TypeName)(publicKeyPropositionEd25519Codec.asDecoder)

  implicit val signatureCurve25519JsonEncoder: Encoder[SignatureCurve25519] = deriveEncoderFromScodec(
    signatureCurve25519TypeName
  )(signatureCurve25519Codec.asEncoder)

  implicit val signatureCurve25519JsonKeyEncoder: KeyEncoder[SignatureCurve25519] = deriveKeyEncoderFromScodec(
    signatureCurve25519TypeName
  )(signatureCurve25519Codec.asEncoder)

  implicit val signatureCurve25519JsonDecoder: Decoder[SignatureCurve25519] =
    deriveDecoderFromScodec(signatureCurve25519TypeName)(signatureCurve25519Codec.asDecoder)

  implicit val signatureCurve25519JsonKeyDecoder: KeyDecoder[SignatureCurve25519] =
    deriveKeyDecoderFromScodec(signatureCurve25519TypeName)(signatureCurve25519Codec.asDecoder)

  implicit val thresholdSignatureCurve25519JsonEncoder: Encoder[ThresholdSignatureCurve25519] =
    deriveEncoderFromScodec(thresholdSignatureCurve25519TypeName)(thresholdSignatureCurve25519Codec.asEncoder)

  implicit val thresholdSignatureCurve25519JsonKeyEncoder: KeyEncoder[ThresholdSignatureCurve25519] =
    deriveKeyEncoderFromScodec(thresholdSignatureCurve25519TypeName)(thresholdSignatureCurve25519Codec.asEncoder)

  implicit val thresholdSignatureCurve25519JsonDecoder: Decoder[ThresholdSignatureCurve25519] =
    deriveDecoderFromScodec(thresholdSignatureCurve25519TypeName)(thresholdSignatureCurve25519Codec.asDecoder)

  implicit val thresholdSignatureCurve25519JsonKeyDecoder: KeyDecoder[ThresholdSignatureCurve25519] =
    deriveKeyDecoderFromScodec(thresholdSignatureCurve25519TypeName)(thresholdSignatureCurve25519Codec.asDecoder)

  implicit val signatureEd25519JsonEncoder: Encoder[SignatureEd25519] =
    deriveEncoderFromScodec(signatureEd25519TypeName)(signatureEd25519Codec.asEncoder)

  implicit val signatureEd25519JsonKeyEncoder: KeyEncoder[SignatureEd25519] =
    deriveKeyEncoderFromScodec(signatureEd25519TypeName)(signatureEd25519Codec.asEncoder)

  implicit val signatureEd25519JsonDecoder: Decoder[SignatureEd25519] =
    deriveDecoderFromScodec(signatureEd25519TypeName)(signatureEd25519Codec.asDecoder)

  implicit val signatureEd25519JsonKeyDecoder: KeyDecoder[SignatureEd25519] =
    deriveKeyDecoderFromScodec(signatureEd25519TypeName)(signatureEd25519Codec.asDecoder)

  implicit def propositionJsonKeyEncoder[P <: Proposition]: KeyEncoder[P] =
    deriveKeyEncoderFromScodec(propositionTypeName)(propositionCodec.asEncoder)

  implicit val propositionJsonKeyDecoder: KeyDecoder[_ <: Proposition] =
    deriveKeyDecoderFromScodec(propositionTypeName)(propositionCodec.asDecoder)
}
