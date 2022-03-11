package co.topl.attestation

import co.topl.attestation.keyManagement.{PrivateKeyCurve25519, PrivateKeyEd25519, Secret}
import co.topl.codecs.binary.legacy.attestation.ProofSerializer
import co.topl.codecs.binary.legacy.{BifrostSerializer, BytesSerializable}
import co.topl.crypto.Signature
import co.topl.crypto.signing.{Curve25519, Ed25519}
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, Proofs, VerificationKeys}
import co.topl.utils.encode.Base58
import com.google.common.primitives.Ints

import scala.util.Try

/**
 * The most general abstraction of fact a prover can provide a non-interactive proof
 * to open a box or to modify an account
 *
 * A proof is non-interactive and thus serializable
 */
sealed trait Proof[P <: Proposition] extends BytesSerializable {

  def isValid(proposition: P, message: Array[Byte]): Boolean

  @deprecated
  type M = Proof[_ <: Proposition]

  @deprecated
  override def serializer: BifrostSerializer[Proof[_ <: Proposition]] = ProofSerializer

  override def toString: String = Base58.encode(bytes)

  override def equals(obj: Any): Boolean = obj match {
    case pr: Proof[_] =>
      bytes sameElements pr.bytes
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(bytes)

}

object Proof {

  sealed trait ProofFromDataError
  case class InvalidBase58() extends ProofFromDataError
  case class ProofParseFailure(error: Throwable) extends ProofFromDataError
  case class ParsedIncorrectSignatureType() extends ProofFromDataError

}

trait ProofInstances {}

/** The proof for a given type of `Secret` and `KnowledgeProposition` */
sealed trait ProofOfKnowledge[S <: Secret, P <: KnowledgeProposition[S]] extends Proof[P]

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

/**
 * A proof corresponding to a PublicKeyCurve25519 proposition. This is a zero-knowledge proof that argues knowledge of
 * the underlying private key associated with a public key
 *
 * @param sigBytes 25519 signature
 */
case class SignatureCurve25519(sigBytes: Signature)
    extends ProofOfKnowledge[PrivateKeyCurve25519, PublicKeyPropositionCurve25519] {

  private val curve25519 = new Curve25519()
  private val signatureLength = sigBytes.value.length

  require(
    signatureLength == 0 || signatureLength == Curve25519.instance.SignatureLength,
    s"$signatureLength != ${Curve25519.instance.SignatureLength}"
  )

  def isValid(proposition: PublicKeyPropositionCurve25519, message: Array[Byte]): Boolean =
    curve25519.verify(
      Proofs.Knowledge.Curve25519(Sized.strictUnsafe[Bytes, Proofs.Knowledge.Curve25519.Length](Bytes(sigBytes.value))),
      Bytes(message),
      VerificationKeys.Curve25519(
        Sized.strictUnsafe[Bytes, VerificationKeys.Curve25519.Length](Bytes(proposition.pubKeyBytes.value))
      )
    )
}

object SignatureCurve25519 {
  lazy val signatureSize: Int = Curve25519.instance.SignatureLength

  /** Helper function to create empty signatures */
  lazy val empty: SignatureCurve25519 = SignatureCurve25519(Signature(Array.emptyByteArray))

  /** Returns a signature filled with 1's for use in genesis signatures */
  lazy val genesis: SignatureCurve25519 =
    SignatureCurve25519(Signature(Array.fill(SignatureCurve25519.signatureSize)(1: Byte)))

}

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

case class ThresholdSignatureCurve25519(signatures: Set[SignatureCurve25519])
    extends ProofOfKnowledge[PrivateKeyCurve25519, ThresholdPropositionCurve25519] {

  val curve25519 = new Curve25519()

  signatures.foreach { sig =>
    require(sig.sigBytes.value.length == SignatureCurve25519.signatureSize)
  }

  override def isValid(proposition: ThresholdPropositionCurve25519, message: Array[Byte]): Boolean = Try {
    // check that we have at least m signatures
    // JAA - the Try wraps this to expression so this check may prematurely exit evaluation and return false
    //       (i.e. the check should fail quickly)
    require(proposition.pubKeyProps.size >= proposition.threshold)

    // only need to check until the threshold is exceeded
    val numValidSigs = signatures.foldLeft((0, proposition.pubKeyProps)) { case ((acc, unusedProps), sig) =>
      if (acc < proposition.threshold) {
        unusedProps
          .find(prop =>
            unusedProps(prop) && curve25519.verify(
              Proofs.Knowledge.Curve25519(Sized.strictUnsafe(Bytes(sig.sigBytes.value))),
              Bytes(message),
              VerificationKeys.Curve25519(Sized.strictUnsafe(Bytes(prop.pubKeyBytes.value)))
            )
          ) match {
          case Some(prop) =>
            (acc + 1, unusedProps.diff(Set(prop)))
          case None =>
            (acc, unusedProps)
        }
      } else (acc, unusedProps)
    }

    require(numValidSigs._1 >= proposition.threshold)
  }.isSuccess

}

object ThresholdSignatureCurve25519 {

  /** Helper function to create empty signatures */
  lazy val empty: ThresholdSignatureCurve25519 = ThresholdSignatureCurve25519(Set[SignatureCurve25519]())

  lazy val genesis: ThresholdSignatureCurve25519 = ThresholdSignatureCurve25519(
    Set[SignatureCurve25519](SignatureCurve25519(Signature(Array.fill(SignatureCurve25519.signatureSize)(1: Byte))))
  )

}

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

case class SignatureEd25519(sigBytes: Signature)
    extends ProofOfKnowledge[PrivateKeyEd25519, PublicKeyPropositionEd25519] {

  private val ed25519 = new Ed25519()
  private val signatureLength = sigBytes.value.length

  require(
    signatureLength == 0 || signatureLength == ed25519.SignatureLength,
    s"$signatureLength != ${ed25519.SignatureLength}"
  )

  def isValid(proposition: PublicKeyPropositionEd25519, message: Array[Byte]): Boolean =
    ed25519.verify(
      Proofs.Knowledge.Ed25519(Sized.strictUnsafe(Bytes(sigBytes.value))),
      Bytes(message),
      VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(proposition.pubKeyBytes.value)))
    )
}

object SignatureEd25519 {
  lazy val signatureSize: Int = Ed25519.instance.SignatureLength

  /** Helper function to create empty signatures */
  lazy val empty: SignatureEd25519 = SignatureEd25519(Signature(Array.emptyByteArray))

  /** Returns a signature filled with 1's for use in genesis signatures */
  lazy val genesis: SignatureEd25519 =
    SignatureEd25519(Signature(Array.fill(Ed25519.instance.SignatureLength)(1: Byte)))

}
