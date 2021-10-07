package co.topl.crypto.signing.kes

import co.topl.crypto.hash.{digest, Blake2b, Blake2bHash, Hash}
import co.topl.crypto.hash.digest.Digest32
import co.topl.crypto.signing.eddsa.Ed25519

import java.security.SecureRandom

trait KesEd25519Blake2b256 {

  val hash: Array[Byte] => Array[Byte] = { (input: Array[Byte]) =>
    import digest.implicits._
    val blake2b256: Hash[Blake2b, Digest32] = new Blake2bHash[Digest32] {}
    blake2b256.hash(input).value
  }
  val sig: Ed25519 = new Ed25519
  val seedBytes: Int = 32
  val pkBytes: Int = 32
  val skBytes: Int = 32
  val sigBytes: Int = 64
  val hashBytes: Int = 32
  val asymmetricLogL: Int = 7
  val symmetricLogL: Int = 9
  val pkLength: Int = hashBytes

  protected lazy val exp_symmetricLogL: Int = exp(symmetricLogL)

  lazy val maxSymmetricKeyTimeSteps: Int = exp_symmetricLogL * exp_symmetricLogL

  protected val random = new SecureRandom()

  /**
   * Exponent base two of the argument
   * @param n integer
   * @return 2 to the n
   */

  protected def exp(n: Int): Int =
    scala.math.pow(2, n).toInt

  /**
   * Pseudorandom number generator used for seed doubling
   * Input must be non-recoverable from output
   * Each output cannot be used to determine one from the other
   * @param k input seed
   * @return tuple of two new seeds
   */

  protected def PRNG(k: Array[Byte]): (Array[Byte], Array[Byte]) = {
    val r1 = hash(k)
    val r2 = hash(r1 ++ k)
    (r1, r2)
  }

  /**
   * generates a keypair for underlying SIG functionality returns it in a single byte array
   * @param seed input entropy for keypair generation
   * @return byte array sk||pk
   */

  protected def sKeypairFast(seed: Array[Byte]): Array[Byte] = {
    val sk = hash(seed)
    val pk = Array.fill(32)(0x00.toByte)
    sig.generatePublicKey(sk, 0, pk, 0)
    sk ++ pk
  }

  /**
   * Signing routine for underlying SIG functionality
   * @param m message to be signed
   * @param sk SIG secret key to be signed
   * @return SIG signature
   */

  protected def sSign(m: Array[Byte], sk: Array[Byte]): Array[Byte] = {
    val signature: Array[Byte] = Array.fill(sigBytes)(0x00.toByte)
    sig.sign(sk, 0, m, 0, m.length, signature, 0)
    signature
  }

  /**
   * Verify routine for underlying SIG functionality
   * @param m message for given signature
   * @param signature signature to be verified
   * @param pk public key corresponding to signature
   * @return true if valid signature, false if otherwise
   */

  protected def sVerify(m: Array[Byte], signature: Array[Byte], pk: Array[Byte]): Boolean =
    sig.verify(signature, 0, pk, 0, m, 0, m.length)

}
