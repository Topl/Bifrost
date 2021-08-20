package co.topl.crypto.signatures

import co.topl.crypto.hash.sha256
import co.topl.crypto.{PrivateKey, PublicKey}

import java.security.SecureRandom

class Ed25519VRF extends eddsa.ECVRF25519 with EllipticCurveSignatureScheme {
  override val SignatureLength: Int = SIGNATURE_SIZE
  override val KeyLength: Int = SECRET_KEY_SIZE

  override def createKeyPair(seed: Array[Byte]): (PrivateKey, PublicKey) = {
    val sk: Array[Byte] = new Array[Byte](SECRET_KEY_SIZE)
    val pk: Array[Byte] = new Array[Byte](PUBLIC_KEY_SIZE)
    val hashedSeed = sha256.hash(seed)
    val random = SecureRandom.getInstance("SHA1PRNG")

    random.setSeed(hashedSeed.value)

    generatePrivateKey(random, sk)
    generatePublicKey(sk, 0, pk, 0)
    (PrivateKey(sk), PublicKey(pk))
  }

  override def sign(privateKey: PrivateKey, message: MessageToSign): Signature = {
    require(privateKey.value.length == SECRET_KEY_SIZE)
    Signature(vrfProof(privateKey.value,message))
  }

  override def verify(signature: Signature, message: MessageToSign, publicKey: PublicKey): Boolean =
    signature.value.length == SIGNATURE_SIZE &&
    publicKey.value.length == PUBLIC_KEY_SIZE &&
    vrfVerify(publicKey.value,message,signature.value)

  def proofToHash(signature: Signature):Array[Byte] = {
    vrfProofToHash(signature.value)
  }
}

object Ed25519VRF {
  val SignatureLength: Int = 64
  val KeyLength: Int = 32
  val HashLength:Int = 64
}
