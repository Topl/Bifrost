package co.topl.utils

import co.topl.crypto.hash.{Digest32, Digest64}
import co.topl.crypto.signatures.{PrivateKey, PublicKey, Signature}

import scala.language.implicitConversions

trait BytesOf[A] {
  def get(value: A): Array[Byte]

  def from(bytes: Array[Byte]): A

  def sameElements[B: BytesOf](a: A, b: B): Boolean = get(a) sameElements BytesOf[B].get(b)

  def concat[B: BytesOf](a: A, b: B): Array[Byte] = get(a) ++ BytesOf[B].get(b)

  def prepend(a: A, byte: Byte): Array[Byte] = byte +: get(a)

  def isEmpty(a: A): Boolean = get(a).isEmpty

  def length(a: A): Int = get(a).length

  def take(a: A, num: Int): Array[Byte] = get(a).take(num)

  def slice(a: A, from: Int, to: Int): Array[Byte] = get(a).slice(from, to)

  def map[B](a: A, f: Byte => B): List[B] = get(a).map(f).toList
}

object BytesOf {
  def apply[T: BytesOf]: BytesOf[T] = implicitly[BytesOf[T]]

  object Implicits {

    implicit val bytesOfDigest32: BytesOf[Digest32] = new BytesOf[Digest32] {
      override def get(value: Digest32): Array[Byte] = value.value

      override def from(bytes: Array[Byte]): Digest32 = Digest32(bytes)
    }

    implicit val bytesOfDigest64: BytesOf[Digest64] = new BytesOf[Digest64] {
      override def get(value: Digest64): Array[Byte] = value.value

      override def from(bytes: Array[Byte]): Digest64 = Digest64(bytes)
    }

    implicit val bytesOfSignature: BytesOf[Signature] = new BytesOf[Signature] {
      override def get(value: Signature): Array[Byte] = value.value

      override def from(bytes: Array[Byte]): Signature = Signature(bytes)
    }

    implicit val bytesOfPublicKey: BytesOf[PublicKey] = new BytesOf[PublicKey] {
      override def get(value: PublicKey): Array[Byte] = value.value

      override def from(bytes: Array[Byte]): PublicKey = PublicKey(bytes)
    }

    implicit def unwrapBytesOf[B: BytesOf](b: B): Array[Byte] = BytesOf[B].get(b)

    implicit val bytesOfArrayBytes: BytesOf[Array[Byte]] = new BytesOf[Array[Byte]] {
      override def get(bytes:  Array[Byte]): Array[Byte] = bytes
      override def from(bytes: Array[Byte]): Array[Byte] = bytes
    }

    implicit val bytesOfString: BytesOf[String] = new BytesOf[String] {
      override def get(value:  String): Array[Byte] = value.getBytes
      override def from(bytes: Array[Byte]): String = new String(bytes)
    }

    implicit val bytesOfPrivateKey: BytesOf[PrivateKey] = new BytesOf[PrivateKey] {
      override def get(value: PrivateKey): Array[Byte] = value.value

      override def from(bytes: Array[Byte]): PrivateKey = PrivateKey(bytes)
    }
  }
}
