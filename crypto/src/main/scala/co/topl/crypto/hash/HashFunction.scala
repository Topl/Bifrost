package co.topl.crypto.hash

import scala.util.Try

/* Forked from https://github.com/input-output-hk/scrypto */

trait HashFunction[T] {
  type Message = Array[Byte]

  val digestSize: Int

  def apply(input: Message): T
  def apply(prefix: Byte, inputs: Array[Byte]*): T
  def byteArrayToDigest(bytes: Array[Byte]): Try[T]
}


