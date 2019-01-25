package bifrost.transaction.proof

import bifrost.transaction.box.proposition.MofNProposition
import com.google.common.primitives.Ints
import bifrost.serialization.Serializer
import bifrost.transaction.box.proposition.{Proposition, PublicKey25519Proposition}
import bifrost.transaction.proof.{ProofOfKnowledge, Signature25519}
import bifrost.transaction.state.PrivateKey25519
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.util.Try

case class MultiSignature25519(signatureSet: Set[Signature25519])
  extends ProofOfKnowledge[PrivateKey25519, MofNProposition] {
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

  override def serializer: Serializer[MultiSignature25519] = MultiSignature25519Serializer

  override def toString: String = s"MultiSignature25519(${
    signatureSet.tail.map(s => Base58.encode(s.signature))
      .foldLeft(Base58.encode(signatureSet.head.signature))(_ + ", " + _)
  })"
}

object MultiSignature25519Serializer extends Serializer[MultiSignature25519] {
  override def toBytes(obj: MultiSignature25519): Array[Byte] =
    Ints.toByteArray(obj.signatureSet.size) ++
      obj
        .signatureSet
        .foldLeft(Array[Byte]())((total, sig) => total ++ sig.signature)

  override def parseBytes(bytes: Array[Byte]): Try[MultiSignature25519] = Try {
    val numSignatures = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val signatureSet: Set[Signature25519] = (0 until numSignatures).map {
      i =>
        Signature25519(bytes.slice(Ints.BYTES + i * MultiSignature25519.SignatureSize,
                                   Ints.BYTES + (i + 1) * MultiSignature25519.SignatureSize))
    }.toSet

    MultiSignature25519(signatureSet)
  }
}

object MultiSignature25519 {
  lazy val SignatureSize = Signature25519.SignatureSize
}
