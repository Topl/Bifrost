package co.topl.crypto.kes

import co.topl.crypto.hash.digest.Digest32
import co.topl.crypto.hash.{Blake2b, Blake2bHash, Hash, digest}
import co.topl.crypto.signing.eddsa.Ed25519
import co.topl.crypto.signing.kes.KesEd25519Blake2b256

/**
 * AMS 2021:
 * Key evolving scheme instantiated with Ed25519 and b2b256 fast cryptographic hash using MMM construction
 */

class KeyEvolvingSignatureScheme extends KesEd25519Blake2b256 {
  override val hash: Array[Byte] => Array[Byte] = { (input: Array[Byte]) =>
    import digest.implicits._
    val blake2b256: Hash[Blake2b, Digest32] = new Blake2bHash[Digest32] {}
    blake2b256.hash(input).value
  }
  override val sig: Ed25519 = new Ed25519
  override val seedBytes: Int = 32
  override val pkBytes: Int = 32
  override val skBytes: Int = 32
  override val sigBytes: Int = 64
  override val hashBytes: Int = 32
  override val asymmetricLogL: Int = 7
  override val symmetricLogL: Int = 9
  override val pkLength: Int = hashBytes
}
