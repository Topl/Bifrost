package co.topl.primitives

import java.security.SecureRandom
import org.bouncycastle.crypto.generators.Ed25519KeyPairGenerator
import org.bouncycastle.crypto.params.{Ed25519KeyGenerationParameters, Ed25519PrivateKeyParameters, Ed25519PublicKeyParameters}
import org.bouncycastle.math.ec.rfc8032.Ed25519

/**
  * AMS 2020:
  * Ledger signing routine using Ed25519
  * This is the only functionality needed for issuing transactions and tracking stake across accounts,
  * Any account with a public field corresponding to a constant F_SIG public key is accessible with the corresponding F_SIG private key alone
  * F_SIG functionality
  */

class Sig {

  val signatureLength = Ed25519.SIGNATURE_SIZE
  val keyLength = Ed25519.PUBLIC_KEY_SIZE

  /**
    * Generate a keypair from seed for Ed25519
    * @param seed
    * @return
    */
  def createKeyPair(seed: Array[Byte]): (Array[Byte],Array[Byte]) = {
    //This algorithm uses SHA-1 as the foundation of the PRNG. It computes the SHA-1 hash over a true-random seed value
    // concatenated with a 64-bit counter which is incremented by 1 for each operation.
    // From the 160-bit SHA-1 output, only 64 bits are used.
    val rnd: SecureRandom = SecureRandom.getInstance("SHA1PRNG")
    rnd.setSeed(seed)
    val kpg = new Ed25519KeyPairGenerator
    kpg.init(new Ed25519KeyGenerationParameters(rnd))
    val kp = kpg.generateKeyPair
    val sk = kp.getPrivate.asInstanceOf[Ed25519PrivateKeyParameters].getEncoded
    val pk = kp.getPublic.asInstanceOf[Ed25519PublicKeyParameters].getEncoded
    (sk,pk)
  }

  def getPkFromSk(sk:Array[Byte]):Array[Byte] = {
    var pk_out = Array.fill(keyLength){0x00.toByte}
    Ed25519.generatePublicKey(sk,0,pk_out,0)
    pk_out
  }

  def sign(privateKey: Array[Byte], message: Array[Byte]): Array[Byte] = {
    var sig:Array[Byte] = Array.fill(signatureLength){0x00.toByte}
    Ed25519.sign(privateKey,0,message,0,message.length,sig,0)
    sig
  }

  def verify(signature: Array[Byte], message: Array[Byte], publicKey: Array[Byte]): Boolean = {
    Ed25519.verify(signature,0,publicKey,0,message,0,message.length) && signature.length == signatureLength && publicKey.length == keyLength
  }

}
