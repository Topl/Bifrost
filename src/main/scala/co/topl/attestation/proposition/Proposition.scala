package co.topl.attestation.proposition

import co.topl.attestation.Address
import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.attestation.secrets.Secret
import co.topl.modifier.transaction.Transaction
import co.topl.utils.serialization.{BifrostSerializer, BytesSerializable}
import com.google.common.primitives.Ints
import scorex.util.encode.Base58

import scala.util.Try

// Propositions are challenges that must be satisfied by the prover.
// In most cases, propositions are used by transactions issuers (spenders) to prove the right
// to use a UTXO in a transaction.
sealed trait Proposition extends BytesSerializable {

  override type M = Proposition
  override def serializer: BifrostSerializer[Proposition] = PropositionSerializer

  def address(implicit networkPrefix: NetworkPrefix): Address

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
  def fromString (str: String): Try[_ <: Proposition] =
    Base58.decode(str).flatMap(bytes => PropositionSerializer.parseBytes(bytes))

  def getPropTypeString(tx: Transaction[_,_,_,_]): String = tx.getPropositionType match {
    case _: PublicKeyPropositionCurve25519 => "PublicKeyCurve25519"
    case _: ThresholdPropositionCurve25519 => "ThresholdCurve25519"
  }
}
