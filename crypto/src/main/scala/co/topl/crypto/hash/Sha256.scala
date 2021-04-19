package co.topl.crypto.hash

import java.security.MessageDigest

/* Forked from https://github.com/input-output-hk/scrypto */

/** Hashing functions implementation with sha256 impl from Java SDK */
object Sha256 extends CryptographicHash32 {
  override def hash(input: Array[Byte]): Digest32 = Digest32 @@ MessageDigest.getInstance("SHA-256").digest(input)
}
