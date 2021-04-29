package co.topl

import co.topl.crypto.BytesOf
import co.topl.crypto.hash.{Blake2b256, Digest32, Hash}

package object utils {
  type HashScheme = Blake2b256
  type HashDigest = Digest32

  def blake2b256[T: BytesOf](value: T): HashDigest = Hash[HashScheme, HashDigest].hash(value)
}
