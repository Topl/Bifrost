package co.topl.attestation.proof

import co.topl.attestation.Proposition
import co.topl.attestation.proposition.{ThresholdCurve25519Proposition, PublicKeyCurve25519Proposition}
import co.topl.attestation.secrets.PrivateKeyCurve25519
import co.topl.utils.serialization.BifrostSerializer
import scorex.crypto.signatures.Curve25519
import scorex.util.encode.Base58

case class ThresholdSignatureCurve25519 (signatures: Set[SignatureCurve25519]) extends ProofOfKnowledge[PrivateKeyCurve25519, ThresholdCurve25519Proposition] {

  signatures.foreach(sig => {
    require(sig.signature.length == ThresholdSignatureCurve25519.SignatureSize)
  })

  override def isValid(proposition: Proposition, message: Array[Byte]): Boolean = proposition match {
    case mn: ThresholdCurve25519Proposition =>
      mn.setOfPubKeyBytes.size >= mn.threshold && // check that we have at least m signatures
        signatures.foldLeft(0)((total, sig) => { // check that at least m signatures are valid
          if (mn.setOfPubKeyBytes.exists(pubKeyBytes => Curve25519.verify(sig.signature, message, pubKeyBytes))) {
            total + 1
          } else {
            total
          }
        }) >= mn.threshold
    case pp: PublicKeyCurve25519Proposition =>
      signatures.exists(sig => Curve25519.verify(sig.signature, message, pp.pubKeyBytes))
    case _                                  => false
  }

  override def toString: String = s"MultiSignature25519(${
    signatures.tail.map(s => Base58.encode(s.signature))
      .foldLeft(Base58.encode(signatures.head.signature))(_ + ", " + _)
  })"
}

object ThresholdSignatureCurve25519 {
  lazy val SignatureSize: Int = SignatureCurve25519.SignatureSize
}
