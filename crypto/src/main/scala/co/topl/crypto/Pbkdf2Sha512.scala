package co.topl.crypto

import org.bouncycastle.crypto.digests.SHA512Digest
import org.bouncycastle.crypto.generators.PKCS5S2ParametersGenerator
import org.bouncycastle.crypto.params.KeyParameter

/**
 * PBKDF-SHA512 defines a function for creating a public key from a password and salt.
 *
 * It repeats the HMAC-SHA512 hashing function a given number of iterations and then slices a number of bytes off the
 * result.
 */
object Pbkdf2Sha512 {

  /**
   * Generates a public key from the given message and salt.
   *
   * Runs HMAC-SHA512 a given number of iterations and creates a key of given size.
   *
   * @param password the password to create a public key for
   * @param salt the salt to apply
   * @param keySizeBytes the resulting key size in bytes
   * @param iterations the number of iterations to run
   * @return the bytes of the key result
   */
  def generateKey(password: Array[Byte], salt: Array[Byte], keySizeBytes: Int, iterations: Int): Array[Byte] =
    synchronized {
      val generator = new PKCS5S2ParametersGenerator(new SHA512Digest)

      generator.init(password, salt, iterations)

      generator.generateDerivedParameters(keySizeBytes * 8).asInstanceOf[KeyParameter].getKey
    }
}
