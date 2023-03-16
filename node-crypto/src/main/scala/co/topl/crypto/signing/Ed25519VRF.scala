package co.topl.crypto.signing

import co.topl.crypto.generation.EntropyToSeed
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.signing.eddsa.ECVRF25519

import java.security.SecureRandom

class Ed25519VRF {

  private val impl = new ECVRF25519
  impl.precompute()

  def generateRandom: (Array[Byte], Array[Byte]) = {
    val random = SecureRandom.getInstance("SHA1PRNG")
    val seed = random.generateSeed(32)
    random.nextBytes(seed) // updating random seed per https://howtodoinjava.com/java8/secure-random-number-generation/
    deriveKeyPairFromSeed(seed)
  }

  def deriveKeyPairFromEntropy(entropy: Entropy, password: Option[String])(implicit
    entropyToSeed: EntropyToSeed = EntropyToSeed.instances.pbkdf2Sha512(32)
  ): (Array[Byte], Array[Byte]) = {
    val seed = entropyToSeed.toSeed(entropy, password)
    deriveKeyPairFromSeed(seed.toArray)
  }

  /**
   * @param seed length = 32
   */
  def deriveKeyPairFromSeed(seed: Array[Byte]): (Array[Byte], Array[Byte]) =
    seed -> getVerificationKey(seed)

  /**
   * @param privateKey length = 32
   * @return length = 80
   */
  def sign(privateKey: Array[Byte], message: Array[Byte]): Array[Byte] =
    impl.vrfProof(privateKey, message)

  /**
   * @param signature length = 80
   * @param publicKey length = 32
   */
  def verify(
    signature: Array[Byte],
    message:   Array[Byte],
    publicKey: Array[Byte]
  ): Boolean =
    impl.vrfVerify(publicKey, message, signature)

  /**
   * @param secretKey length = 32
   * @return length = 32
   */
  def getVerificationKey(secretKey: Array[Byte]): Array[Byte] = {
    val pkByteVector = new Array[Byte](impl.PUBLIC_KEY_SIZE)
    impl.generatePublicKey(secretKey.toArray, 0, pkByteVector, 0)
    pkByteVector
  }

  /**
   * @param signature length = 80
   * @return length = 64
   */
  def proofToHash(signature: Array[Byte]): Array[Byte] =
    impl.vrfProofToHash(signature.toArray)

}

object Ed25519VRF {

  def precomputed(): Ed25519VRF =
    new Ed25519VRF
}
