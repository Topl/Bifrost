package co.topl.utils

import cats.Eq
import co.topl.crypto.hash.{Digest32, Digest64}
import co.topl.crypto.signatures.{PrivateKey, PublicKey, Signature}
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

  implicit val stringAsBytes: AsBytes[String] = (value: String) => value.getBytes

  implicit val privateKeyAsBytes: AsBytes[PrivateKey] = (value: PrivateKey) => value.value
}

trait AsBytesImplicitExtensions {
  implicit def eqAsBytes[A: AsBytes]: Eq[A] = (x: A, y: A) => AsBytes[A].asBytes(x) sameElements AsBytes[A].asBytes(y)

  implicit def unwrapAsBytes[B: AsBytes](b: B): Array[Byte] = AsBytes[B].asBytes(b)
}

object AsBytes {
  object implicits extends AsBytesInstances with AsBytesImplicitExtensions with ToAsBytesOps
}
