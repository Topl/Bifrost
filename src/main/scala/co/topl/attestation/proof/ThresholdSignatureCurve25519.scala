package co.topl.attestation.proof

import co.topl.attestation.proposition.{ PublicKeyCurve25519Proposition, ThresholdCurve25519Proposition }
import co.topl.attestation.secrets.PrivateKeyCurve25519
import io.circe.{ Decoder, Encoder, KeyDecoder, KeyEncoder }
import scorex.crypto.signatures.{ Curve25519, PublicKey, Signature }
import scorex.util.encode.Base58

import scala.util.{ Failure, Success, Try }

case class ThresholdSignatureCurve25519 (private[proof] val signatures: Set[SignatureCurve25519])
  extends ProofOfKnowledge[PrivateKeyCurve25519, ThresholdCurve25519Proposition] {

  signatures.foreach(sig => {
    require(sig.sigBytes.length == ThresholdSignatureCurve25519.SignatureSize)
  })

  override def isValid(proposition: ThresholdCurve25519Proposition, message: Array[Byte]): Boolean = Try {
    // check that we have at least m signatures
    // JAA - the Try wraps this to expression so this check may prematurely exit evaluation and return false
    //       (i.e. the check should fail quickly)
    require(proposition.pubKeyProps.size >= proposition.threshold)

    // todo: JAA - could improve this by not checking all sigs (only need to check until the threshold is exceeded)
    val numValidSigs = signatures.map { sig =>
      proposition.pubKeyProps.exists(prop => Curve25519.verify(sig.sigBytes, message, PublicKey @@ prop.bytes))
    }.count(_ == true)

    require(numValidSigs >= proposition.threshold)

  }.isSuccess
}

object ThresholdSignatureCurve25519 {
  lazy val SignatureSize: Int = SignatureCurve25519.SignatureSize

  def apply (sigStr: String): ThresholdSignatureCurve25519 =
    proofDecoder.decodeString(sigStr) match {
      case Success(sig: ThresholdSignatureCurve25519) => sig
      case Failure(ex)                                => throw ex
    }

  /** Helper function to create empty signatures */
  def empty (): ThresholdSignatureCurve25519 = ThresholdSignatureCurve25519(Set[SignatureCurve25519]())

  implicit val proofEncoder: ProofEncoder[ThresholdCurve25519Proposition, Proof[ThresholdCurve25519Proposition]] =
    ProofEncoder.instance { tSig => Base58.encode(tSig.bytes) }

  implicit val proofDecoder: ProofDecoder[ThresholdCurve25519Proposition, Proof[ThresholdCurve25519Proposition]] =
    ProofDecoder.instance { sigStr: String => Base58.decode(sigStr).flatMap(SignatureCurve25519Serializer.parseBytes) }

  // see circe documentation for custom encoder / decoders
  // https://circe.github.io/circe/codecs/custom-codecs.html
  implicit val jsonEncoder: Encoder[ThresholdSignatureCurve25519] =
  (sig: ThresholdSignatureCurve25519) => proofEncoder.encodeString(sig).asJson

  implicit val jsonKeyEncoder: KeyEncoder[ThresholdSignatureCurve25519] =
    (sig: ThresholdSignatureCurve25519) => proofEncoder.encodeString(sig)

  implicit val jsonDecoder: Decoder[ThresholdSignatureCurve25519] =
    Decoder.decodeString.emapTry(proofDecoder.decodeString)

  implicit val jsonKeyDecoder: KeyDecoder[ThresholdSignatureCurve25519] =
    (str: String) => Some(ThresholdSignatureCurve25519(str))
}
