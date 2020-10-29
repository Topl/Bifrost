package co.topl.attestation.proof

import co.topl.attestation.proposition.PublicKeyCurve25519Proposition
import co.topl.attestation.secrets.PrivateKeyCurve25519
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}
import scorex.util.encode.Base58

import scala.util.{Failure, Success, Try}

/**
  * @param sigBytes 25519 signature
  */
case class SignatureCurve25519 (private[proof] val sigBytes: Signature)
  extends ProofOfKnowledge[PrivateKeyCurve25519, PublicKeyCurve25519Proposition] {

  require(sigBytes.isEmpty || sigBytes.length == Curve25519.SignatureLength,
    s"${sigBytes.length} != ${Curve25519.SignatureLength}")

  def isValid (proposition: PublicKeyCurve25519Proposition, message: Array[Byte]): Boolean =
    Curve25519.verify(sigBytes, message, PublicKey @@ proposition.bytes)

  override def toString: String = SignatureCurve25519.proofEncoder.encodeString(this)
}




object SignatureCurve25519 {
  lazy val SignatureSize: Int = Curve25519.SignatureLength

  def apply (sigStr: String): SignatureCurve25519 =
    proofDecoder.decodeString(sigStr) match {
      case Success(sig: SignatureCurve25519) => sig
      case Failure(ex)                       => throw ex
    }

  /** Returns a signature filled with 1's for use in genesis signatures */
  def genesis (): SignatureCurve25519 = {
    SignatureCurve25519(Signature @@ Array.fill(SignatureCurve25519.SignatureSize)(1: Byte))
  }

  /** Helper function to create empty signatures */
  def empty (): SignatureCurve25519 = SignatureCurve25519(Signature @@ Array.emptyByteArray)

  implicit val proofEncoder: ProofEncoder[PublicKeyCurve25519Proposition, Proof[PublicKeyCurve25519Proposition]] =
    ProofEncoder.instance { sig => Base58.encode(sig.bytes) }

  implicit val proofDecoder: ProofDecoder[PublicKeyCurve25519Proposition, Proof[PublicKeyCurve25519Proposition]] =
    ProofDecoder.instance { sigStr: String => Base58.decode(sigStr).flatMap(SignatureCurve25519Serializer.parseBytes) }

  // see circe documentation for custom encoder / decoders
  // https://circe.github.io/circe/codecs/custom-codecs.html
  implicit val jsonEncoder: Encoder[SignatureCurve25519] = (sig: SignatureCurve25519) => proofEncoder.encodeString(sig).asJson
  implicit val jsonKeyEncoder: KeyEncoder[SignatureCurve25519] = (sig: SignatureCurve25519) => proofEncoder.encodeString(sig)
  implicit val jsonDecoder: Decoder[SignatureCurve25519] = Decoder.decodeString.emapTry(proofDecoder.decodeString)
  implicit val jsonKeyDecoder: KeyDecoder[SignatureCurve25519] = (str: String) => Some(SignatureCurve25519(str))
}
