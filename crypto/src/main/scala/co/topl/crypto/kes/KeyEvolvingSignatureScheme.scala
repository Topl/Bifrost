package co.topl.crypto.kes

import co.topl.crypto.hash.digest.Digest32
import co.topl.crypto.hash.{digest, Blake2bHash}
import co.topl.crypto.signing.kes.{MMM, Sig}

/**
 * AMS 2021:
 * Key evolving scheme instantiated with Ed25519 and b2b256 fast cryptographic hash using MMM construction
 */

class KeyEvolvingSignatureScheme extends MMM[Blake2bHash[Digest32], Sig] {
  import digest.implicits._
  override val fch: Blake2bHash[Digest32] = new Blake2bHash[Digest32] {}
  override val sig: Sig = new Sig
  override val seedBytes: Int = 32
  override val pkBytes: Int = 32
  override val skBytes: Int = 32
  override val sigBytes: Int = 64
  override val hashBytes: Int = 32
  override val asymmetricLogL: Int = 7
  override val symmetricLogL: Int = 9
  override val pkLength: Int = hashBytes
}
