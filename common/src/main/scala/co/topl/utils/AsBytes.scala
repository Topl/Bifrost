package co.topl.utils

import cats.Eq
import co.topl.crypto.hash.digest.Digest
import co.topl.crypto.hash.digest.implicits._
import co.topl.crypto.signatures.{PrivateKey, PublicKey, Signature}
import simulacrum.typeclass

import scala.language.implicitConversions

@typeclass
trait AsBytes[A] {
  def asBytes(value: A): Array[Byte]
}

trait AsBytesInstances {
  implicit def digestAsBytes[T: Digest]: AsBytes[T] = (value: T) => value.bytes

  implicit val signatureAsBytes: AsBytes[Signature] = (value: Signature) => value.value

  implicit val publicKeyAsBytes: AsBytes[PublicKey] = (value: PublicKey) => value.value

  implicit val arrayBytesAsBytes: AsBytes[Array[Byte]] = (bytes: Array[Byte]) => bytes

  implicit val privateKeyAsBytes: AsBytes[PrivateKey] = (value: PrivateKey) => value.value
}

trait AsBytesImplicitExtensions {
  implicit def eqAsBytes[A: AsBytes]: Eq[A] = (x: A, y: A) => AsBytes[A].asBytes(x) sameElements AsBytes[A].asBytes(y)
}

object AsBytes {
  object implicits extends AsBytesInstances with AsBytesImplicitExtensions with ToAsBytesOps
}
