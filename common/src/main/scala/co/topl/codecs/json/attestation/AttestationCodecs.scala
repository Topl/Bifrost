package co.topl.codecs.json.attestation

import cats.implicits._
import co.topl.attestation._
import co.topl.attestation.AddressCodec.implicits._
import co.topl.codecs.binary._
import co.topl.codecs.binary.typeclasses.Transmittable
import co.topl.codecs.json.valuetypes._
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.StringDataTypes.implicits._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

trait AttestationCodecs extends co.topl.codecs.json.attestation.keyManagement.KeyManagementCodecs {
  implicit val addressJsonEncoder: Encoder[Address] = (addr: Address) => addr.toString.asJson

  implicit val addressJsonKeyEncoder: KeyEncoder[Address] = (addr: Address) => addr.toString

  implicit def addressJsonDecoder(implicit networkPrefix: NetworkPrefix): Decoder[Address] =
    Decoder[Base58Data].emap(_.decodeAddress.toEither.leftMap(_.toString))

  implicit def addressJsonKeyDecoder(implicit networkPrefix: NetworkPrefix): KeyDecoder[Address] =
    json => Base58Data.validated(json).toOption.flatMap(_.decodeAddress.toOption)

  implicit val evidenceJsonEncoder: Encoder[Evidence] = _.transmittableBase58.asJson

  implicit val evidenceJsonKeyEncoder: KeyEncoder[Evidence] = _.transmittableBase58.show

  implicit val evidenceJsonDecoder: Decoder[Evidence] =
    Decoder[Base58Data].emap(_.decodeTransmitted[Evidence])

  implicit val evidenceJsonKeyDecoder: KeyDecoder[Evidence] =
    KeyDecoder[Base58Data].map(_.decodeTransmitted[Evidence].getOrThrow())

  implicit def proofJsonEncoder[P <: Proposition]: Encoder[Proof[P]] = value =>
    Transmittable[Proof[_ <: Proposition]].transmittableBase58(value).asJson

  implicit val proofJsonDecoder: Decoder[Proof[_ <: Proposition]] =
    Decoder[Base58Data].emap(_.decodeTransmitted[Proof[_ <: Proposition]])

  implicit val publicKeyPropositionCurve25519JsonEncoder: Encoder[PublicKeyPropositionCurve25519] =
    _.transmittableBase58.asJson

  implicit val publicKeyPropositionCurve25519JsonKeyEncoder: KeyEncoder[PublicKeyPropositionCurve25519] =
    _.transmittableBase58.show

  implicit val publicKeyPropositionCurve25519JsonDecoder: Decoder[PublicKeyPropositionCurve25519] =
    Decoder[Base58Data].emap(_.value.decodeTransmitted[PublicKeyPropositionCurve25519])

  implicit val publciKeyPropositionCurve25519JsonKeyDecoder: KeyDecoder[PublicKeyPropositionCurve25519] =
    KeyDecoder[Base58Data].map(_.value.decodeTransmitted[PublicKeyPropositionCurve25519].getOrThrow())

  implicit val thresholdPropositionCurve25519JsonEncoder: Encoder[ThresholdPropositionCurve25519] =
    _.transmittableBase58.asJson

  implicit val thresholdPropositionCurve25519JsonKeyEncoder: KeyEncoder[ThresholdPropositionCurve25519] =
    _.transmittableBase58.show

  implicit val thresholdPropositionCurve25519JsonDecoder: Decoder[ThresholdPropositionCurve25519] =
    Decoder[Base58Data].emap(_.decodeTransmitted[ThresholdPropositionCurve25519])

  implicit val thresholdPropositionCurve25519JsonKeyDecoder: KeyDecoder[ThresholdPropositionCurve25519] =
    KeyDecoder[Base58Data].map(_.decodeTransmitted[ThresholdPropositionCurve25519].getOrThrow())

  implicit val publicKeyPropositionEd25519JsonEncoder: Encoder[PublicKeyPropositionEd25519] =
    _.transmittableBase58.asJson

  implicit val publicKeyPropositionEd25519JsonKeyEncoder: KeyEncoder[PublicKeyPropositionEd25519] =
    _.transmittableBase58.show

  implicit val publicKeyPropositionEd25519JsonDecoder: Decoder[PublicKeyPropositionEd25519] =
    Decoder[Base58Data].emap(_.decodeTransmitted[PublicKeyPropositionEd25519])

  implicit val publicKeyPropositionEd25519JsonKeyDecoder: KeyDecoder[PublicKeyPropositionEd25519] =
    KeyDecoder[Base58Data].map(_.decodeTransmitted[PublicKeyPropositionEd25519].getOrThrow())

  implicit val signatureCurve25519JsonEncoder: Encoder[SignatureCurve25519] = _.transmittableBase58.asJson

  implicit val signatureCurve25519JsonKeyEncoder: KeyEncoder[SignatureCurve25519] = _.transmittableBase58.show

  implicit val signatureCurve25519JsonDecoder: Decoder[SignatureCurve25519] =
    Decoder[Base58Data].emap(_.decodeTransmitted[SignatureCurve25519])

  implicit val signatureCurve25519JsonKeyDecoder: KeyDecoder[SignatureCurve25519] =
    KeyDecoder[Base58Data].map(_.decodeTransmitted[SignatureCurve25519].getOrThrow())

  implicit val thresholdSignatureCurve25519JsonEncoder: Encoder[ThresholdSignatureCurve25519] =
    _.transmittableBase58.asJson

  implicit val thresholdSignatureCurve25519JsonKeyEncoder: KeyEncoder[ThresholdSignatureCurve25519] =
    _.transmittableBase58.show

  implicit val thresholdSignatureCurve25519JsonDecoder: Decoder[ThresholdSignatureCurve25519] =
    Decoder[Base58Data].emap(_.decodeTransmitted[ThresholdSignatureCurve25519])

  implicit val thresholdSignatureCurve25519JsonKeyDecoder: KeyDecoder[ThresholdSignatureCurve25519] =
    KeyDecoder[Base58Data].map(_.decodeTransmitted[ThresholdSignatureCurve25519].getOrThrow())

  implicit val signatureEd25519JsonEncoder: Encoder[SignatureEd25519] = _.transmittableBase58.asJson

  implicit val signatureEd25519JsonKeyEncoder: KeyEncoder[SignatureEd25519] = _.transmittableBase58.show

  implicit val signatureEd25519JsonDecoder: Decoder[SignatureEd25519] =
    Decoder[Base58Data].emap(_.decodeTransmitted[SignatureEd25519])

  implicit val signatureEd25519JsonKeyDecoder: KeyDecoder[SignatureEd25519] =
    KeyDecoder[Base58Data].map(_.decodeTransmitted[SignatureEd25519].getOrThrow())

  implicit def propositionJsonKeyEncoder[P <: Proposition]: KeyEncoder[P] =
    value => Transmittable[Proposition].transmittableBase58(value).show

  implicit def propositionJsonKeyDecoder: KeyDecoder[Proposition] =
    KeyDecoder[Base58Data].map(_.decodeTransmitted[Proposition].getOrThrow())
}
