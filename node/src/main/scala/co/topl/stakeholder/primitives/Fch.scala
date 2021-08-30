package co.topl.stakeholder.primitives

import co.topl.crypto.hash.{Blake2b, Blake2bHash, Hash, digest, Message}
import co.topl.crypto.hash.digest.Digest32

/**
  * AMS 2020:
  * Fast Crypto Hash as a class for actor system
  * Primary hash algorithm system wide
  * G_RO functionality
  */

class Fch {
  import digest.implicits._

  val blake2b256: Hash[Blake2b, Digest32] = new Blake2bHash[Digest32] {}

  def hash(input: Message): Array[Byte] = blake2b256.hash(input).value
  def hash(input: String): Array[Byte] = blake2b256.hash(input.getBytes).value
}