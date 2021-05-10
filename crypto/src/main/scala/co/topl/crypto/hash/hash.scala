package co.topl.crypto

import co.topl.crypto.hash.digest._

import scala.language.implicitConversions

/* Forked from https://github.com/input-output-hk/scrypto */

package object hash {

  abstract class Hash[H, D: Digest] {
    def hash(prefix: Option[Byte], messages: Array[Byte]*): D

    def hash(prefix: Byte, messages: Array[Byte]*): D = hash(Some(prefix), messages: _*)

    def hash(message: Array[Byte]): D = hash(None, message)
  }

  object Hash {
    def apply[H, D: Digest](implicit hash: Hash[H, D]): Hash[H, D] = hash
  }

  type Blake2b
  type Sha

  trait instances {
    implicit val sha256: Hash[Sha, Digest32] = Sha256
    implicit val sha512: Hash[Sha, Digest64] = Sha512
    implicit val blake2b256: Hash[Blake2b, Digest32] = Blake2b256
    implicit val blake2b512: Hash[Blake2b, Digest64] = Blake2b512
  }

  object implicits extends digest.instances with digest.Digest.ToDigestOps with instances
}
