package co.topl.attestation

import cats.Show
import cats.data.NonEmptyChain
import cats.implicits._
import co.topl.attestation.Evidence.{EvidenceContent, EvidenceTypePrefix}
import co.topl.attestation.keyManagement.{PrivateKeyCurve25519, Secret}
import co.topl.attestation.serialization.PropositionSerializer
import co.topl.crypto.PublicKey
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.hash.implicits._
import co.topl.crypto.signatures.Curve25519
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.codecs.implicits._
import co.topl.utils.StringDataTypes.implicits._
import co.topl.utils.StringDataTypes.{Base58Data, DataEncodingValidationFailure}
import co.topl.utils.serialization.{BifrostSerializer, BytesSerializable}
import co.topl.utils.{Identifiable, Identifier}
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

import scala.collection.SortedSet

// Propositions are challenges that must be satisfied by the prover.
// In most cases, propositions are used by transactions issuers (spenders) to prove the right
// to use a UTXO in a transaction.
sealed trait Proposition extends BytesSerializable {

  override type M = Proposition
  override def serializer: BifrostSerializer[Proposition] = PropositionSerializer

  def address(implicit networkPrefix: NetworkPrefix): Address

  override def toString: String = bytes.encodeAsBase58.show

  override def equals(obj: Any): Boolean = obj match {
    case prop: Proposition => prop.bytes sameElements bytes
    case _                 => false
  }

  override def hashCode(): Int = Ints.fromByteArray(bytes)
}

object Proposition {

  sealed trait PropositionFromDataFailure

  final case class IncorrectEncoding(error: NonEmptyChain[DataEncodingValidationFailure])
      extends PropositionFromDataFailure
  final case class BytesParsingError(error: Throwable) extends PropositionFromDataFailure

  def fromString(str: String): Either[PropositionFromDataFailure, _ <: Proposition] =
    Base58Data.validated(str).leftMap(IncorrectEncoding).toEither.flatMap(fromBase58)

  def fromBase58(data: Base58Data): Either[PropositionFromDataFailure, _ <: Proposition] =
    PropositionSerializer.parseBytes(data.value).toEither.leftMap(BytesParsingError)

  implicit def jsonKeyEncoder[P <: Proposition]: KeyEncoder[P] = (prop: P) => prop.toString

  implicit val jsonKeyDecoder: KeyDecoder[Proposition] =
    json => Base58Data.validated(json).toOption.flatMap(fromBase58(_).toOption)

  implicit val showPropositionFromStringFailure: Show[PropositionFromDataFailure] = {
    case IncorrectEncoding(errors) => s"String is an incorrect encoding type: $errors"
    case BytesParsingError(error)  => s"Failed to parse the decoded bytes: $error"
  }
}

// Knowledge propositions require the prover to supply a proof attesting to their knowledge
// of secret information.
sealed trait KnowledgeProposition[S <: Secret] extends Proposition

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

case class PublicKeyPropositionCurve25519(private[attestation] val pubKeyBytes: PublicKey)
    extends KnowledgeProposition[PrivateKeyCurve25519] {

  require(
    pubKeyBytes.value.length == Curve25519.KeyLength,
    s"Incorrect pubKey length, ${Curve25519.KeyLength} expected, ${pubKeyBytes.value.length} found"
  )

  def address(implicit networkPrefix: NetworkPrefix): Address = Address.from(this)

}

object PublicKeyPropositionCurve25519 {
  // type prefix used for address creation
  val typePrefix: EvidenceTypePrefix = 1: Byte
  val typeString: String = "PublicKeyCurve25519"

  def apply(str: String): PublicKeyPropositionCurve25519 =
    Proposition.fromString(str) match {
      case Right(pk: PublicKeyPropositionCurve25519) => pk
      case Right(_)                                  => throw new Error("Invalid proposition generation")
      case Left(failure) =>
        throw new Error(s"Failed to create Public Key Curve 25519 proposition from string: ${failure.show}")
    }

  def fromBase58(data: Base58Data): PublicKeyPropositionCurve25519 =
    Proposition.fromBase58(data) match {
      case Right(pk: PublicKeyPropositionCurve25519) => pk
      case Right(_)                                  => throw new Error("Invalid proposition generation")
      case Left(failure) =>
        throw new Error(s"Failed to create Public Key Curve 25519 proposition from string: ${failure.show}")
    }

  implicit val ord: Ordering[PublicKeyPropositionCurve25519] = Ordering.by(_.toString)

  implicit val evProducer: EvidenceProducer[PublicKeyPropositionCurve25519] =
    EvidenceProducer.instance[PublicKeyPropositionCurve25519] { prop: PublicKeyPropositionCurve25519 =>
      Evidence(typePrefix, EvidenceContent(blake2b256.hash(prop.bytes.tail)))
    }

  implicit val identifier: Identifiable[PublicKeyPropositionCurve25519] = Identifiable.instance { () =>
    Identifier(typeString, typePrefix)
  }

  // see circe documentation for custom encoder / decoders
  // https://circe.github.io/circe/codecs/custom-codecs.html
  implicit val jsonEncoder: Encoder[PublicKeyPropositionCurve25519] = (prop: PublicKeyPropositionCurve25519) =>
    prop.toString.asJson

  implicit val jsonKeyEncoder: KeyEncoder[PublicKeyPropositionCurve25519] = (prop: PublicKeyPropositionCurve25519) =>
    prop.toString
  implicit val jsonDecoder: Decoder[PublicKeyPropositionCurve25519] = Decoder[Base58Data].map(fromBase58)
  implicit val jsonKeyDecoder: KeyDecoder[PublicKeyPropositionCurve25519] = KeyDecoder[Base58Data].map(fromBase58)
}

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

case class ThresholdPropositionCurve25519(threshold: Int, pubKeyProps: SortedSet[PublicKeyPropositionCurve25519])
    extends KnowledgeProposition[PrivateKeyCurve25519] {

  pubKeyProps.foreach { prop =>
    require(
      prop.pubKeyBytes.value.length == Curve25519.KeyLength,
      s"Incorrect pubKey length, ${Curve25519.KeyLength} expected, ${prop.pubKeyBytes.value.length} found"
    )
  }

//  val propTypeString: String = ThresholdPropositionCurve25519.typeString
//  val propTypePrefix: EvidenceTypePrefix = ThresholdPropositionCurve25519.typePrefix

  def address(implicit networkPrefix: NetworkPrefix): Address = Address.from(this)

}

object ThresholdPropositionCurve25519 {
  // type prefix used for address creation
  val typePrefix: EvidenceTypePrefix = 2: Byte
  val typeString: String = "ThresholdCurve25519"

  def apply(str: String): ThresholdPropositionCurve25519 =
    Proposition.fromString(str) match {
      case Right(prop: ThresholdPropositionCurve25519) => prop
      case Right(_)                                    => throw new Error("Invalid proposition generation")
      case Left(failure) =>
        throw new Error(s"Failed to create Threshold Curve 25519 proposition from string: ${failure.show}")
    }

  def fromBase58(data: Base58Data): ThresholdPropositionCurve25519 =
    Proposition.fromBase58(data) match {
      case Right(prop: ThresholdPropositionCurve25519) => prop
      case Right(_)                                    => throw new Error("Invalid proposition generation")
      case Left(failure) =>
        throw new Error(s"Failed to create Threshold Curve 25519 proposition from string: ${failure.show}")
    }

  implicit val evProducer: EvidenceProducer[ThresholdPropositionCurve25519] =
    EvidenceProducer.instance[ThresholdPropositionCurve25519] { prop: ThresholdPropositionCurve25519 =>
      Evidence(typePrefix, EvidenceContent(blake2b256.hash(prop.bytes.tail)))
    }

  implicit val identifier: Identifiable[ThresholdPropositionCurve25519] = Identifiable.instance { () =>
    Identifier(typeString, typePrefix)
  }

  // see circe documentation for custom encoder / decoders
  // https://circe.github.io/circe/codecs/custom-codecs.html
  implicit val jsonEncoder: Encoder[ThresholdPropositionCurve25519] = (prop: ThresholdPropositionCurve25519) =>
    prop.toString.asJson

  implicit val jsonKeyEncoder: KeyEncoder[ThresholdPropositionCurve25519] = (prop: ThresholdPropositionCurve25519) =>
    prop.toString
  implicit val jsonDecoder: Decoder[ThresholdPropositionCurve25519] = Decoder[Base58Data].map(fromBase58)
  implicit val jsonKeyDecoder: KeyDecoder[ThresholdPropositionCurve25519] = KeyDecoder[Base58Data].map(fromBase58)
}
