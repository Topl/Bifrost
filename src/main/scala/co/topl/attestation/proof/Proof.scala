package co.topl.attestation.proof

import co.topl.attestation.proposition.{ KnowledgeProposition, Proposition, PropositionSerializer }
import co.topl.attestation.secrets.Secret
import co.topl.utils.serialization.{ BifrostSerializer, BytesSerializable }
import com.google.common.primitives.Ints
import io.circe.{ Encoder, KeyEncoder }
import scorex.util.encode.Base58

import scala.util.{ Failure, Success, Try }

/**
  * The most general abstraction of fact a prover can provide a non-interactive proof
  * to open a box or to modify an account
  *
  * A proof is non-interactive and thus serializable
  */
sealed trait Proof[P <: Proposition] extends BytesSerializable {

  def isValid(proposition: P, message: Array[Byte]): Boolean

  override type M = Proof[_ <: Proposition]
  override def serializer: BifrostSerializer[Proof[_ <: Proposition]] = ProofSerializer

  override def toString: String = Base58.encode(bytes)

  override def equals (obj: Any): Boolean = obj match {
    case pr: Proof[P] => pr.bytes sameElements bytes
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(bytes)

}

object Proof {
  def fromString[P <: Proposition, PR <: Proof[P]] (str: String): Try[PR] =
    Base58.decode(str).flatMap(bytes => ProofSerializer.parseBytes(bytes) match {
      case Success(prop: PR) => Success(prop)
      case _                => Failure(new Error("Failed to parse a proposition from the given string"))
    })

  implicit def jsonEncoder[P <: Proposition]: Encoder[Proof[P]] = {
    case pr: SignatureCurve25519          => SignatureCurve25519.jsonEncoder(pr)
    case pr: ThresholdSignatureCurve25519 => ThresholdSignatureCurve25519.jsonEncoder(pr)
  }

  implicit def jsonKeyEncoder[P <: Proposition]: KeyEncoder[Proof[P]] = {
    case pr: SignatureCurve25519          => SignatureCurve25519.jsonKeyEncoder(pr)
    case pr: ThresholdSignatureCurve25519 => ThresholdSignatureCurve25519.jsonKeyEncoder(pr)
  }
}

trait ProofOfKnowledge[S <: Secret, P <: KnowledgeProposition[S]] extends Proof[P]
