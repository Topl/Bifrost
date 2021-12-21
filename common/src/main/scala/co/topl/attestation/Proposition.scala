package co.topl.attestation

import cats.Show
import cats.data.NonEmptyChain
import cats.implicits._
import co.topl.attestation.Evidence.{EvidenceContent, EvidenceTypePrefix}
import co.topl.attestation.keyManagement.{PrivateKeyCurve25519, PrivateKeyEd25519, Secret}
import co.topl.codecs._
import co.topl.codecs.binary.legacy.attestation.PropositionSerializer
import co.topl.codecs.binary.legacy.{BifrostSerializer, BytesSerializable}
import co.topl.crypto.PublicKey
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.signatures.{Curve25519, Ed25519}
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.implicits.showBase16String
import co.topl.utils.StringDataTypes.{Base16Data, DataEncodingValidationFailure}
import co.topl.utils.{Identifiable, Identifier}
import com.google.common.primitives.Ints

import scala.collection.SortedSet

// Propositions are challenges that must be satisfied by the prover.
// In most cases, propositions are used by transactions issuers (spenders) to prove the right
// to use a UTXO in a transaction.
sealed trait Proposition extends BytesSerializable {

  @deprecated
  type M = Proposition

  @deprecated
  override def serializer: BifrostSerializer[Proposition] = PropositionSerializer

  def address(implicit networkPrefix: NetworkPrefix): Address

  override def toString: String = Base16Data.fromData(bytes).show

  override def equals(obj: Any): Boolean = obj match {
    case prop: Proposition => bytes sameElements prop.bytes
    case _                 => false
  }

  override def hashCode(): Int = Ints.fromByteArray(bytes)
}

object Proposition {

  sealed trait PropositionFromDataFailure

  final case class IncorrectEncoding(error: NonEmptyChain[DataEncodingValidationFailure])
      extends PropositionFromDataFailure
  final case class BytesParsingError(error: Throwable) extends PropositionFromDataFailure

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
}
