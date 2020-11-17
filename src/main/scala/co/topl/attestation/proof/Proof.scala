package co.topl.attestation.proof

import co.topl.attestation.proposition.{KnowledgeProposition, Proposition}
import co.topl.attestation.secrets.Secret
import co.topl.utils.serialization.{BifrostSerializer, BytesSerializable}
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import scorex.util.encode.Base58

import scala.util.Try

/**
  * The most general abstraction of fact a prover can provide a non-interactive proof
  * to open a box or to modify an account
  *
  * A proof is non-interactive and thus serializable
  */
sealed trait Proof[P <: Proposition] extends BytesSerializable {

  def isValid(proposition: P, message: Array[Byte]): Boolean

  override type M = Proof[_]
  override def serializer: BifrostSerializer[Proof[_]] = ProofSerializer

  override def toString: String = Base58.encode(bytes)

  override def equals (obj: Any): Boolean = obj match {
    case pr: Proof[_] => pr.bytes sameElements bytes
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(bytes)

}

trait ProofOfKnowledge[S <: Secret, P <: KnowledgeProposition[S]] extends Proof[P]


object Proof {
  def fromString(str: String): Try[Proof[_]] =
    Base58.decode(str).flatMap(bytes => ProofSerializer.parseBytes(bytes))

  implicit def jsonEncoder[PR <: Proof[_]]: Encoder[PR] = (proof: PR) => proof.toString.asJson
  implicit def jsonDecoder: Decoder[Proof[_]] = Decoder.decodeString.map((str: String) => fromString(str).get)
}
