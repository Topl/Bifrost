package co.topl.crypto

import co.topl.crypto.accumulators.LeafData
import co.topl.crypto.hash.{Digest, Digest32, Digest64}
import co.topl.crypto.signatures.{PrivateKey, PublicKey, Signature}

import scala.language.implicitConversions

object Implicits {
  implicit def unwrapFancyBytes[B: BytesOf](b: B): Array[Byte] = BytesOf[B].get(b)

  implicit val bytesOfArrayBytes: BytesOf[Array[Byte]] = new BytesOf[Array[Byte]] {
    override def get(bytes:  Array[Byte]): Array[Byte] = bytes
    override def from(bytes: Array[Byte]): Array[Byte] = bytes
  }

  implicit val bytesOfString: BytesOf[String] = new BytesOf[String] {
    override def get(value:  String): Array[Byte] = value.getBytes
    override def from(bytes: Array[Byte]): String = new String(bytes)
  }

  implicit val bytesOfPublicKey: BytesOf[PublicKey] = new BytesOf[PublicKey] {
    override def get(value: PublicKey): Array[Byte] = value.value

    override def from(bytes: Array[Byte]): PublicKey = PublicKey(bytes)
  }

  implicit val bytesOfPrivateKey: BytesOf[PrivateKey] = new BytesOf[PrivateKey] {
    override def get(value: PrivateKey): Array[Byte] = value.value

    override def from(bytes: Array[Byte]): PrivateKey = PrivateKey(bytes)
  }

  implicit val bytesOfSignature: BytesOf[Signature] = new BytesOf[Signature] {
    override def get(value: Signature): Array[Byte] = value.value

    override def from(bytes: Array[Byte]): Signature = Signature(bytes)
  }

  implicit val digestDigest64: Digest[Digest64] = new Digest[Digest64] {
    override val size: Int = Digest64.size

    override def from[B: BytesOf](b: B): Digest64 = Digest64(BytesOf[B].get(b))
  }

  implicit val bytesOfDigest64: BytesOf[Digest64] = new BytesOf[Digest64] {
    override def get(value: Digest64): Array[Byte] = value.value

    override def from(bytes: Array[Byte]): Digest64 = Digest64(bytes)
  }

  implicit val digestDigest32: Digest[Digest32] = new Digest[Digest32] {
    override val size: Int = Digest32.size

    override def from[B: BytesOf](b: B): Digest32 = Digest32(BytesOf[B].get(b))
  }

  implicit val bytesOfDigest32: BytesOf[Digest32] = new BytesOf[Digest32] {
    override def get(value: Digest32): Array[Byte] = value.value

    override def from(bytes: Array[Byte]): Digest32 = Digest32(bytes)
  }

  implicit val bytesOfLeafData: BytesOf[LeafData] = new BytesOf[LeafData] {
    override def get(value: LeafData): Array[Byte] = value.value

    override def from(bytes: Array[Byte]): LeafData = LeafData(bytes)
  }
}
