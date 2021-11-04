package co.topl.attestation

import cats.implicits.toShow
import co.topl.attestation.keyManagement.{PrivateKeyCurve25519, PrivateKeyEd25519, Secret}
import co.topl.crypto.signatures.{Curve25519, Ed25519, Signature}
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.StringDataTypes.implicits.showBase58String
import co.topl.utils.codecs._
import co.topl.utils.codecs.binary.legacy.BifrostSerializer
import co.topl.utils.codecs.binary.legacy.attestation.ProofSerializer
import co.topl.utils.codecs.binary.typeclasses.Transmittable
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

import scala.util.Try

/**
 * The most general abstraction of fact a prover can provide a non-interactive proof
 * to open a box or to modify an account
 *
 * A proof is non-interactive and thus serializable
 */
sealed trait Proof[P <: Proposition] {

  def isValid(proposition: P, message: Array[Byte]): Boolean

  def serializer: BifrostSerializer[Proof[_]] = ProofSerializer

  override def toString: String = Transmittable[Proof[_]].transmittableBase58(this).show

  override def equals(obj: Any): Boolean = obj match {
    case pr: Proof[_] =>
      Transmittable[Proof[_]].transmittableBytes(this) sameElements Transmittable[Proof[_]].transmittableBytes(pr)
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(Transmittable[Proof[_]].transmittableBytes(this))

}

object Proof {

  sealed trait ProofFromDataError
  case class InvalidBase58() extends ProofFromDataError
  case class ProofParseFailure(error: Throwable) extends ProofFromDataError
  case class ParsedIncorrectSignatureType() extends ProofFromDataError

  implicit def jsonEncoder[P <: Proposition]: Encoder[Proof[P]] = value =>
    Transmittable[Proof[_]].transmittableBase58(value).asJson

  implicit def jsonDecoder: Decoder[Proof[_]] =
    Decoder[Base58Data].emap(data => data.decodeTransmitted[Proof[_]])
}

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

  private val signatureLength = sigBytes.value.length

  require(
    signatureLength == 0 || signatureLength == Curve25519.SignatureLength,
    s"$signatureLength != ${Curve25519.SignatureLength}"
  )

  def isValid(proposition: PublicKeyPropositionCurve25519, message: Array[Byte]): Boolean =
    Curve25519.verify(sigBytes, message, proposition.pubKeyBytes)
}

object SignatureCurve25519 {
  lazy val signatureSize: Int = Curve25519.SignatureLength

  /** Helper function to create empty signatures */
  lazy val empty: SignatureCurve25519 = SignatureCurve25519(Signature(Array.emptyByteArray))

  /** Returns a signature filled with 1's for use in genesis signatures */
  lazy val genesis: SignatureCurve25519 =
    SignatureCurve25519(Signature(Array.fill(SignatureCurve25519.signatureSize)(1: Byte)))

  // see circe documentation for custom encoder / decoders
  // https://circe.github.io/circe/codecs/custom-codecs.html
  implicit val jsonEncoder: Encoder[SignatureCurve25519] = _.transmittableBase58.asJson
  implicit val jsonKeyEncoder: KeyEncoder[SignatureCurve25519] = _.transmittableBase58.show

  implicit val jsonDecoder: Decoder[SignatureCurve25519] =
    Decoder[Base58Data].emap(_.value.decodeTransmitted[SignatureCurve25519])

  implicit val jsonKeyDecoder: KeyDecoder[SignatureCurve25519] =
    KeyDecoder[Base58Data].map(_.value.decodeTransmitted[SignatureCurve25519].getOrThrow())
}

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

case class ThresholdSignatureCurve25519(signatures: Set[SignatureCurve25519])
    extends ProofOfKnowledge[PrivateKeyCurve25519, ThresholdPropositionCurve25519] {

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
          .find(prop => unusedProps(prop) && Curve25519.verify(sig.sigBytes, message, prop.pubKeyBytes)) match {
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

  // see circe documentation for custom encoder / decoders
  // https://circe.github.io/circe/codecs/custom-codecs.html
  implicit val jsonEncoder: Encoder[ThresholdSignatureCurve25519] = _.transmittableBase58.asJson

  implicit val jsonKeyEncoder: KeyEncoder[ThresholdSignatureCurve25519] = _.transmittableBase58.show

  implicit val jsonDecoder: Decoder[ThresholdSignatureCurve25519] =
    Decoder[Base58Data].emap(_.value.decodeTransmitted[ThresholdSignatureCurve25519])

  implicit val jsonKeyDecoder: KeyDecoder[ThresholdSignatureCurve25519] =
    KeyDecoder[Base58Data].map(_.value.decodeTransmitted[ThresholdSignatureCurve25519].getOrThrow())
}

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

case class SignatureEd25519(sigBytes: Signature)
    extends ProofOfKnowledge[PrivateKeyEd25519, PublicKeyPropositionEd25519] {

  private val signatureLength = sigBytes.value.length
  private val ec = new Ed25519

  require(
    signatureLength == 0 || signatureLength == Ed25519.SignatureLength,
    s"$signatureLength != ${Ed25519.SignatureLength}"
  )

  def isValid(proposition: PublicKeyPropositionEd25519, message: Array[Byte]): Boolean =
    ec.verify(sigBytes, message, proposition.pubKeyBytes)
}

object SignatureEd25519 {
  lazy val signatureSize: Int = Ed25519.SignatureLength

  /** Helper function to create empty signatures */
  lazy val empty: SignatureEd25519 = SignatureEd25519(Signature(Array.emptyByteArray))

  /** Returns a signature filled with 1's for use in genesis signatures */
  lazy val genesis: SignatureEd25519 =
    SignatureEd25519(Signature(Array.fill(SignatureEd25519.signatureSize)(1: Byte)))

  // see circe documentation for custom encoder / decoders
  // https://circe.github.io/circe/codecs/custom-codecs.html
  implicit val jsonEncoder: Encoder[SignatureEd25519] = _.transmittableBase58.asJson
  implicit val jsonKeyEncoder: KeyEncoder[SignatureEd25519] = _.transmittableBase58.show

  implicit val jsonDecoder: Decoder[SignatureEd25519] =
    Decoder[Base58Data].emap(_.value.decodeTransmitted[SignatureEd25519])

  implicit val jsonKeyDecoder: KeyDecoder[SignatureEd25519] =
    KeyDecoder[Base58Data].map(_.value.decodeTransmitted[SignatureEd25519].getOrThrow())
}
