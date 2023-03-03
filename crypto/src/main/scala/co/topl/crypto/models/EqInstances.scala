package co.topl.crypto.models

import cats.Eq

trait EqInstances {

  implicit val verificationKeyKesProductEq: Eq[VerificationKeyKesProduct] = Eq.fromUniversalEquals

  implicit val signatureKesSumeEq: Eq[SignatureKesSum] = Eq.fromUniversalEquals

  implicit val signatureKesProductEq: Eq[SignatureKesProduct] = Eq.fromUniversalEquals
}

object EqInstances extends EqInstances
