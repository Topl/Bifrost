package co.topl.utils

import cats.Eq
import co.topl.crypto.hash.{Digest32, Digest64}
import co.topl.crypto.signatures.{PrivateKey, PublicKey, Signature}
import co.topl.utils.Extensions.StringOps
import co.topl.utils.StringTypes.{Base16String, Base58String, Latin1String, UTF8String}
import simulacrum.typeclass

import scala.language.implicitConversions

@typeclass
trait AsBytes[A] {
  def asBytes(value: A): Array[Byte]
}

trait AsBytesInstances {
  implicit val digest32AsBytes: AsBytes[Digest32] = (value: Digest32) => value.value

  implicit val digest64AsBytes: AsBytes[Digest64] = (value: Digest64) => value.value

  implicit val signatureAsBytes: AsBytes[Signature] = (value: Signature) => value.value

  implicit val publicKeyAsBytes: AsBytes[PublicKey] = (value: PublicKey) => value.value

  implicit val arrayBytesAsBytes: AsBytes[Array[Byte]] = (bytes: Array[Byte]) => bytes

  implicit val privateKeyAsBytes: AsBytes[PrivateKey] = (value: PrivateKey) => value.value

  implicit val latin1StringAsBytes: AsBytes[Latin1String] = (value: Latin1String) =>
    // validity is checked on instantiation
    value.value.getValidLatin1Bytes.get

  implicit val utf8StringAsBytes: AsBytes[UTF8String] = (value: UTF8String) =>
    // validity is checked on instantiation
    value.value.getValidUTF8Bytes.get

  implicit val base58StringAsBytes: AsBytes[Base58String] = (value: Base58String) =>
    // validity is checked on instantiation
    value.value.value.getValidUTF8Bytes.get

  implicit val base16StringAsBytes: AsBytes[Base16String] = (value: Base16String) =>
    // validity is checked on instantiation
    value.value.value.getValidUTF8Bytes.get
}

trait AsBytesImplicitExtensions {
  implicit def eqAsBytes[A: AsBytes]: Eq[A] = (x: A, y: A) => AsBytes[A].asBytes(x) sameElements AsBytes[A].asBytes(y)
}

object AsBytes {
  object implicits extends AsBytesInstances with AsBytesImplicitExtensions with ToAsBytesOps
}
