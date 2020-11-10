package co.topl.attestation.proof

import co.topl.attestation.proposition.ThresholdPropositionCurve25519
import co.topl.attestation.secrets.PrivateKeyCurve25519
import io.circe.syntax.EncoderOps
import io.circe.{ Decoder, Encoder, KeyDecoder, KeyEncoder }
import scorex.crypto.signatures.{ Curve25519, PublicKey }

import scala.util.{ Failure, Success, Try }

case class ThresholdSignatureCurve25519 (private[proof] val signatures: Set[SignatureCurve25519])
  extends ProofOfKnowledge[PrivateKeyCurve25519, ThresholdPropositionCurve25519] {

  signatures.foreach(sig => {
    require(sig.sigBytes.length == ThresholdSignatureCurve25519.SignatureSize)
  })

  override def isValid( proposition: ThresholdPropositionCurve25519, message: Array[Byte]): Boolean = Try {
    // check that we have at least m signatures
    // JAA - the Try wraps this to expression so this check may prematurely exit evaluation and return false
    //       (i.e. the check should fail quickly)
    require(proposition.pubKeyProps.size >= proposition.threshold)

    // only need to check until the threshold is exceeded
    val numValidSigs = signatures.foldLeft(0) { (acc, sig) =>
      if(acc < proposition.threshold) {
        if (proposition.pubKeyProps.exists(prop => Curve25519.verify(sig.sigBytes, message, PublicKey @@ prop.bytes))) {
          1
        } else {
          0
        }
      } else {
        0
      }
    }

    require(numValidSigs >= proposition.threshold)

  }.isSuccess

}

object ThresholdSignatureCurve25519 {
  lazy val SignatureSize: Int = SignatureCurve25519.SignatureSize

  def apply (str: String): ThresholdSignatureCurve25519 =
    Proof.fromString(str) match {
      case Success(sig: ThresholdSignatureCurve25519) => sig
      case Failure(ex)                                => throw ex
    }

  /** Helper function to create empty signatures */
  def empty (): ThresholdSignatureCurve25519 = ThresholdSignatureCurve25519(Set[SignatureCurve25519]())

  // see circe documentation for custom encoder / decoders
  // https://circe.github.io/circe/codecs/custom-codecs.html
  implicit val jsonEncoder: Encoder[ThresholdSignatureCurve25519] = (sig: ThresholdSignatureCurve25519) => sig.toString.asJson
  implicit val jsonKeyEncoder: KeyEncoder[ThresholdSignatureCurve25519] = (sig: ThresholdSignatureCurve25519) => sig.toString
  implicit val jsonDecoder: Decoder[ThresholdSignatureCurve25519] = Decoder.decodeString.map(apply)
  implicit val jsonKeyDecoder: KeyDecoder[ThresholdSignatureCurve25519] = (str: String) => Some(apply(str))
}
