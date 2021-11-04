package co.topl.attestation

import cats.Show
import cats.data.NonEmptyChain
import cats.implicits._
import co.topl.attestation.Evidence.{EvidenceContent, EvidenceTypePrefix}
import co.topl.attestation.keyManagement.{PrivateKeyCurve25519, PrivateKeyEd25519, Secret}
import co.topl.crypto.PublicKey
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.signatures.{Curve25519, Ed25519}
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.implicits.showBase58String
import co.topl.utils.StringDataTypes.{Base58Data, DataEncodingValidationFailure}
import co.topl.utils.codecs._
import co.topl.utils.codecs.binary.legacy.BifrostSerializer
import co.topl.utils.codecs.binary.legacy.attestation.PropositionSerializer
import co.topl.utils.codecs.binary.typeclasses.{BinaryShow, Transmittable}
import co.topl.utils.{Identifiable, Identifier}
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

import scala.collection.SortedSet

// Propositions are challenges that must be satisfied by the prover.
// In most cases, propositions are used by transactions issuers (spenders) to prove the right
// to use a UTXO in a transaction.
sealed trait Proposition {

  def serializer: BifrostSerializer[Proposition] = PropositionSerializer

  def address(implicit networkPrefix: NetworkPrefix): Address

  override def toString: String = BinaryShow[Proposition].encodeAsBase58(this).show

  override def equals(obj: Any): Boolean = obj match {
    case prop: Proposition => Transmittable[Proposition].transmittableBytes(this) sameElements prop.transmittableBytes
    case _                 => false
  }

  override def hashCode(): Int = Ints.fromByteArray(Transmittable[Proposition].transmittableBytes(this))
}

object Proposition {

  sealed trait PropositionFromDataFailure

  final case class IncorrectEncoding(error: NonEmptyChain[DataEncodingValidationFailure])
      extends PropositionFromDataFailure
  final case class BytesParsingError(error: Throwable) extends PropositionFromDataFailure

  implicit def jsonKeyEncoder[P <: Proposition]: KeyEncoder[P] =
    value => Transmittable[Proposition].transmittableBase58(value).show

  implicit def jsonKeyDecoder: KeyDecoder[Proposition] =
    KeyDecoder[Base58Data].map(_.value.decodeTransmitted[Proposition].getOrThrow())

  implicit val showPropositionFromStringFailure: Show[PropositionFromDataFailure] = {
    case IncorrectEncoding(errors) => s"String is an incorrect encoding type: $errors"
    case BytesParsingError(error)  => s"Failed to parse the decoded bytes: $error"
  }
}

// Knowledge propositions require the prover to supply a proof attesting to their knowledge
// of secret information.

sealed trait KnowledgeProposition[S <: Secret] extends Proposition
/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

case class PublicKeyPropositionCurve25519(pubKeyBytes: PublicKey) extends KnowledgeProposition[PrivateKeyCurve25519] {

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

  implicit val ord: Ordering[PublicKeyPropositionCurve25519] = Ordering.by(_.toString)

  implicit val evProducer: EvidenceProducer[PublicKeyPropositionCurve25519] =
    EvidenceProducer.instance[PublicKeyPropositionCurve25519] { prop: PublicKeyPropositionCurve25519 =>
      Evidence(typePrefix, EvidenceContent(blake2b256.hash(prop.persistedBytes).value))
    }

  implicit val identifier: Identifiable[PublicKeyPropositionCurve25519] = Identifiable.instance { () =>
    Identifier(typeString, typePrefix)
  }

  // see circe documentation for custom encoder / decoders
  // https://circe.github.io/circe/codecs/custom-codecs.html
  implicit val jsonEncoder: Encoder[PublicKeyPropositionCurve25519] = _.transmittableBase58.asJson

  implicit val jsonKeyEncoder: KeyEncoder[PublicKeyPropositionCurve25519] = _.transmittableBase58.show

  implicit val jsonDecoder: Decoder[PublicKeyPropositionCurve25519] =
    Decoder[Base58Data].emap(_.value.decodeTransmitted[PublicKeyPropositionCurve25519])

  implicit val jsonKeyDecoder: KeyDecoder[PublicKeyPropositionCurve25519] =
    KeyDecoder[Base58Data].map(_.value.decodeTransmitted[PublicKeyPropositionCurve25519].getOrThrow())
}

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

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

  implicit val evProducer: EvidenceProducer[ThresholdPropositionCurve25519] =
    EvidenceProducer.instance[ThresholdPropositionCurve25519] { prop: ThresholdPropositionCurve25519 =>
      Evidence(typePrefix, EvidenceContent(blake2b256.hash(prop.persistedBytes).value))
    }

  implicit val identifier: Identifiable[ThresholdPropositionCurve25519] = Identifiable.instance { () =>
    Identifier(typeString, typePrefix)
  }

  // see circe documentation for custom encoder / decoders
  // https://circe.github.io/circe/codecs/custom-codecs.html
  implicit val jsonEncoder: Encoder[ThresholdPropositionCurve25519] = _.transmittableBase58.asJson

  implicit val jsonKeyEncoder: KeyEncoder[ThresholdPropositionCurve25519] = _.transmittableBase58.show

  implicit val jsonDecoder: Decoder[ThresholdPropositionCurve25519] =
    Decoder[Base58Data].emap(_.value.decodeTransmitted[ThresholdPropositionCurve25519])

  implicit val jsonKeyDecoder: KeyDecoder[ThresholdPropositionCurve25519] =
    KeyDecoder[Base58Data].map(data => data.value.decodeTransmitted[ThresholdPropositionCurve25519].getOrThrow())
}

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

case class PublicKeyPropositionEd25519(pubKeyBytes: PublicKey) extends KnowledgeProposition[PrivateKeyEd25519] {

  require(
    pubKeyBytes.value.length == Ed25519.KeyLength,
    s"Incorrect pubKey length, ${Ed25519.KeyLength} expected, ${pubKeyBytes.value.length} found"
  )

  def address(implicit networkPrefix: NetworkPrefix): Address = Address.from(this)

}

object PublicKeyPropositionEd25519 {
  // type prefix used for address creation
  val typePrefix: EvidenceTypePrefix = 3: Byte
  val typeString: String = "PublicKeyEd25519"

  implicit val ord: Ordering[PublicKeyPropositionEd25519] = Ordering.by(_.toString)

  implicit val evProducer: EvidenceProducer[PublicKeyPropositionEd25519] =
    EvidenceProducer.instance[PublicKeyPropositionEd25519] { prop: PublicKeyPropositionEd25519 =>
      Evidence(typePrefix, EvidenceContent(blake2b256.hash(prop.persistedBytes).value))
    }

  implicit val identifier: Identifiable[PublicKeyPropositionEd25519] = Identifiable.instance { () =>
    Identifier(typeString, typePrefix)
  }

  // see circe documentation for custom encoder / decoders
  // https://circe.github.io/circe/codecs/custom-codecs.html
  implicit val jsonEncoder: Encoder[PublicKeyPropositionEd25519] = _.transmittableBase58.asJson

  implicit val jsonKeyEncoder: KeyEncoder[PublicKeyPropositionEd25519] = _.transmittableBase58.show

  implicit val jsonDecoder: Decoder[PublicKeyPropositionEd25519] =
    Decoder[Base58Data].emap(_.value.decodeTransmitted[PublicKeyPropositionEd25519])

  implicit val jsonKeyDecoder: KeyDecoder[PublicKeyPropositionEd25519] =
    KeyDecoder[Base58Data].map(_.value.decodeTransmitted[PublicKeyPropositionEd25519].getOrThrow())
}
