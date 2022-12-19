package co.topl.crypto.signing

import co.topl.protobuf.utility.Sized
import co.topl.protobuf.{Bytes, Ed25519Sized}
import co.topl.proto.models.{ProofKnowledgeEd25519, PropositionKnowledgeEd25519, VerificationKeyEd25519}
import com.google.protobuf.ByteString

class Ed25519
    extends EllipticCurveSignatureScheme[
      PropositionKnowledgeEd25519,
      VerificationKeyEd25519,
      ProofKnowledgeEd25519,
      Ed25519Sized.Length
    ] {
  private val impl = new eddsa.Ed25519
  impl.precompute()

  override val SignatureLength: Int = impl.SIGNATURE_SIZE
  override val KeyLength: Int = impl.SECRET_KEY_SIZE

  override def deriveKeyPairFromSeed(
    seed: Sized.Strict[Bytes, Ed25519Sized.Length]
  ): (PropositionKnowledgeEd25519, VerificationKeyEd25519) = {
    val secretKey = PropositionKnowledgeEd25519.of(
      Some(
        VerificationKeyEd25519.of(
          ByteString.copyFrom(
            Ed25519Sized.verificationKeyEd25519(seed.data.toArray)
          )
        )
      )
    )

    val verificationKey = getVerificationKey(secretKey)
    secretKey -> verificationKey
  }

  override def sign(privateKey: PropositionKnowledgeEd25519, message: Bytes): ProofKnowledgeEd25519 = {
    val sig = new Array[Byte](impl.SIGNATURE_SIZE)
    impl.sign(
      privateKey.getKey.value.toByteArray,
      0,
      message.toArray,
      0,
      message.toArray.length,
      sig,
      0
    )

    ProofKnowledgeEd25519.of(
      ByteString.copyFrom(
        Ed25519Sized.proofEd25519(sig)
      )
    )
  }

  override def verify(
    signature: ProofKnowledgeEd25519,
    message:   Bytes,
    publicKey: VerificationKeyEd25519
  ): Boolean = {
    val sigByteArray = signature.value.toByteArray
    val vkByteArray = publicKey.value.toByteArray
    val msgByteArray = message.toArray

    sigByteArray.length == impl.SIGNATURE_SIZE &&
    vkByteArray.length == impl.PUBLIC_KEY_SIZE &&
    impl.verify(
      sigByteArray,
      0,
      vkByteArray,
      0,
      msgByteArray,
      0,
      msgByteArray.length
    )
  }

  override def getVerificationKey(secretKey: PropositionKnowledgeEd25519): VerificationKeyEd25519 = {
    val pkBytes = new Array[Byte](impl.PUBLIC_KEY_SIZE)
    impl.generatePublicKey(secretKey.getKey.value.toByteArray, 0, pkBytes, 0)
    VerificationKeyEd25519.of(
      ByteString.copyFrom(
        Ed25519Sized.verificationKeyEd25519(pkBytes)
      )
    )

  }
}

object Ed25519 {
  val instance = new Ed25519
}
