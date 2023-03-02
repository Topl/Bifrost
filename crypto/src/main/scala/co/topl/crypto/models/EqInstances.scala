package co.topl.crypto.models

import cats.Eq
import cats.implicits._

trait EqInstances {

  implicit val arrayByteEq: Eq[Array[Byte]] =
    (a, b) => a.sameElements(b)

  implicit val verificationKeyKesProductEq: Eq[VerificationKeyKesProduct] =
    (a, b) =>
      a.value === b.value &&
      a.step === b.step

  implicit val signatureKesSumeEq: Eq[SignatureKesSum] =
    (a, b) =>
      a.verificationKey === b.verificationKey &&
      a.signature === b.signature &&
      a.witness === b.witness

  implicit val signatureKesProductEq: Eq[SignatureKesProduct] =
    (a, b) =>
      a.superSignature === b.superSignature &&
      a.subSignature === b.subSignature &&
      a.subRoot === b.subRoot

}

object EqInstances extends EqInstances
