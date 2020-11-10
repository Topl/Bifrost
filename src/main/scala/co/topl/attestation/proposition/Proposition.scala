package co.topl.attestation.proposition

import co.topl.attestation.{ Address, EvidenceProducer }
import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.attestation.proof.{ Proof, SignatureCurve25519 }
import co.topl.attestation.secrets.Secret
import co.topl.modifier.transaction.Transaction
import co.topl.utils.serialization.{ BifrostSerializer, BytesSerializable }
import com.google.common.primitives.Ints
import scorex.util.encode.Base58

import scala.util.{ Failure, Success, Try }

// Propositions are challenges that must be satisfied by the prover.
// In most cases, propositions are used by transactions issuers (spenders) to prove the right
// to use a UTXO in a transaction.
sealed trait Proposition extends BytesSerializable {

  def address(implicit networkPrefix: NetworkPrefix): Address

  override type M = Proposition
  override def serializer: BifrostSerializer[Proposition] = PropositionSerializer

  override def toString: String = Base58.encode(bytes)

  override def equals (obj: Any): Boolean = obj match {
    case prop: Proposition => prop.bytes sameElements bytes
    case _                 => false
  }

  override def hashCode(): Int = Ints.fromByteArray(bytes)

}

// Knowledge propositions require the prover to supply a proof attesting to their knowledge
// of secret information.
trait KnowledgeProposition[S <: Secret] extends Proposition


object Proposition {
  def fromString[P <: Proposition] (str: String): Try[P] =
    Base58.decode(str).flatMap(bytes => PropositionSerializer.parseBytes(bytes) match {
      case Success(prop: P) => Success(prop)
      case _                => Failure(new Error("Failed to parse a proposition from the given string"))
    })

  def getPropTypeString[TX[_,_], P <: Proposition] (obj: TX[P,_]): String = obj match {
    case _: TX[PublicKeyPropositionCurve25519, _] => "PublicKeyCurve25519"
    case _: TX[ThresholdPropositionCurve25519, _] => "ThresholdCurve25519"
  }
}
