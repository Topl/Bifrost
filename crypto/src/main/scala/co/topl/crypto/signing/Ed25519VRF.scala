package co.topl.crypto.signing

import co.topl.crypto.generation.EntropyToSeed
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.signing.eddsa.ECVRF25519
import scodec.bits.ByteVector

import java.security.SecureRandom

class Ed25519VRF {

  private val impl = new ECVRF25519
  impl.precompute()

  def generateRandom: (ByteVector, ByteVector) = {
    val random = SecureRandom.getInstance("SHA1PRNG")
    val seed = random.generateSeed(32)
    random.nextBytes(seed) // updating random seed per https://howtodoinjava.com/java8/secure-random-number-generation/
    deriveKeyPairFromSeed(ByteVector(seed))
  }

  def deriveKeyPairFromEntropy(entropy: Entropy, password: Option[String])(implicit
    entropyToSeed: EntropyToSeed = EntropyToSeed.instances.pbkdf2Sha512(32)
  ): (ByteVector, ByteVector) = {
    val seed = entropyToSeed.toSeed(entropy, password)
    deriveKeyPairFromSeed(seed)
  }

  /**
   * @param seed length = 32
   */
  def deriveKeyPairFromSeed(seed: ByteVector): (ByteVector, ByteVector) =
    seed -> getVerificationKey(seed)

  /**
   * @param privateKey length = 32
   * @return length = 80
   */
  def sign(privateKey: ByteVector, message: ByteVector): ByteVector =
    ByteVector(
      impl.vrfProof(privateKey.toArray, message.toArray)
    )

  /**
   * @param signature length = 80
   * @param publicKey length = 32
   */
  def verify(
    signature: ByteVector,
    message:   ByteVector,
    publicKey: ByteVector
  ): Boolean =
    impl.vrfVerify(publicKey.toArray, message.toArray, signature.toArray)

  /**
   * @param secretKey length = 32
   * @return length = 32
   */
  def getVerificationKey(secretKey: ByteVector): ByteVector = {
    val pkByteVector = new Array[Byte](impl.PUBLIC_KEY_SIZE)
    impl.generatePublicKey(secretKey.toArray, 0, pkByteVector, 0)
    ByteVector(pkByteVector)
  }

  /**
   * @param signature length = 80
   * @return length = 64
   */
  def proofToHash(signature: ByteVector): ByteVector =
    ByteVector(impl.vrfProofToHash(signature.toArray))

}

object Ed25519VRF {

  def precomputed(): Ed25519VRF =
    new Ed25519VRF
}
