package co.topl.crypto

import scorex.crypto.hash.{Blake2b256, CryptographicHash32, Digest32}

/**
 * Interface for fast and secure Blake2b hash function
 */

object FastCryptographicHash extends CryptographicHash32 {
  override def hash(input: Message): Digest32 = Blake2b256.hash(input)
}
