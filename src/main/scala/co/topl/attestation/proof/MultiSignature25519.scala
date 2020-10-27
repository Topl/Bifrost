package co.topl.attestation.proof

import co.topl.attestation.proposition.{ MofNProposition, Proposition, PublicKey25519Proposition }
import co.topl.attestation.proof.serialization.MultiSignature25519Serializer
import co.topl.attestation.secrets.PrivateKey25519
import co.topl.utils.serialization.BifrostSerializer
import scorex.crypto.signatures.Curve25519
import scorex.util.encode.Base58

case class MultiSignature25519(signatureSet: Set[Signature25519]) extends ProofOfKnowledge[PrivateKey25519, MofNProposition] {

  signatureSet.foreach(sig => {
    require(sig.signature.length == MultiSignature25519.SignatureSize)
  })

  override def isValid(proposition: Proposition, message: Array[Byte]): Boolean = proposition match {
    case mn: MofNProposition =>
      mn.setOfPubKeyBytes.size >= mn.m && // check that we have at least m signatures
        signatureSet.foldLeft(0)((total, sig) => { // check that at least m signatures are valid
          if (mn.setOfPubKeyBytes.exists(pubKeyBytes => Curve25519.verify(sig.signature, message, pubKeyBytes))) {
            total + 1
          } else {
            total
          }
        }) >= mn.m
    case pp: PublicKey25519Proposition =>
      signatureSet.exists(sig => Curve25519.verify(sig.signature, message, pp.pubKeyBytes))
    case _ => false
  }

  override type M = MultiSignature25519

  override def serializer: BifrostSerializer[MultiSignature25519] = MultiSignature25519Serializer

  override def toString: String = s"MultiSignature25519(${
    signatureSet.tail.map(s => Base58.encode(s.signature))
      .foldLeft(Base58.encode(signatureSet.head.signature))(_ + ", " + _)
  })"
}

object MultiSignature25519 {
  lazy val SignatureSize: Int = Signature25519.SignatureSize
}
