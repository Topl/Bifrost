package co.topl.models

import co.topl.models.utility.{Lengths, Sized}

sealed trait VerificationKey

object VerificationKeys {

  case class VrfEd25519(bytes: Sized.Strict[Bytes, VrfEd25519.Length]) extends VerificationKey

  object VrfEd25519 {
    type Length = Lengths.`32`.type
  }

  case class KesSum(bytes: Sized.Strict[Bytes, KesSum.Length], step: Int) extends VerificationKey

  object KesSum {
    type Length = Lengths.`32`.type
  }

  case class KesProduct(bytes: Sized.Strict[Bytes, KesProduct.Length], step: Int) extends VerificationKey

  object KesProduct {
    type Length = Lengths.`32`.type
  }
}
