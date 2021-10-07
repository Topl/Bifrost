package co.topl.models

import co.topl.models.utility.{Lengths, Sized}

sealed trait VerificationKey

object VerificationKeys {
  case class Curve25519(bytes: Sized.Strict[Bytes, Curve25519.Length]) extends VerificationKey

  object Curve25519 {
    type Length = Lengths.`32`.type
  }

  case class Ed25519(bytes: Sized.Strict[Bytes, Ed25519.Length]) extends VerificationKey

  object Ed25519 {
    type Length = Lengths.`32`.type
  }

  case class ExtendedEd25519(
    ed25519:   Ed25519,
    chainCode: Sized.Strict[Bytes, ExtendedEd25519.ChainCodeLength]
  ) extends VerificationKey

  object ExtendedEd25519 {
    type Length = Lengths.`32`.type
    type ChainCodeLength = Lengths.`32`.type
  }

  case class VrfEd25519(bytes: Sized.Strict[Bytes, VrfEd25519.Length]) extends VerificationKey

  object VrfEd25519 {
    type Length = Lengths.`32`.type
  }

  case class KesSum(bytes: Array[Byte]) extends VerificationKey

  object KesSum {
    ///type Length = ???
  }

  case class KesSymmetricProduct(bytes: Array[Byte]) extends VerificationKey

  object KesSymmetricProduct {
    ///type Length = ???
  }

  case class KesAsymmetricProduct(bytes: Array[Byte]) extends VerificationKey

  object KesAsymmetricProduct {
    ///type Length = ???
  }

  case class HdKesSum(
//                       xvkM: ExtendedEd25519,
//                       t: Long,
                       bytes: Array[Byte]) extends VerificationKey

  object HdKesSum {
    type Length = Lengths.`32`.type
  }

}
