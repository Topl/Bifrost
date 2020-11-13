package co.topl.attestation.proof

import co.topl.attestation.proposition.{KnowledgeProposition, Proposition}
import co.topl.attestation.secrets.Secret
import co.topl.utils.serialization.{BifrostSerializer, BytesSerializable}
import com.google.common.primitives.Ints
import scorex.util.encode.Base58

import scala.util.{Failure, Success, Try}

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
    case pr: Proof[_] => pr.bytes sameElements bytes
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(bytes)

}

trait ProofOfKnowledge[S <: Secret, P <: KnowledgeProposition[S]] extends Proof[P]


object Proof {
  def fromString[P <: Proposition, PR <: Proof[P]] (str: String): Try[PR] =
    Base58.decode(str).flatMap(bytes => ProofSerializer.parseBytes(bytes).map {
      case prop: PR @unchecked => prop
    })
}
