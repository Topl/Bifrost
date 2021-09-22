package co.topl.models

import co.topl.models.utility.{Lengths, Sized}

sealed trait VerificationKey

object VerificationKeys {
  case class Curve25519(bytes: Sized.Strict[Bytes, Curve25519.Length]) extends VerificationKey

  case class Ed25519(bytes: Sized.Strict[Bytes, Ed25519.Length]) extends VerificationKey

  case class ExtendedEd25519(
    ed25519:   Ed25519,
    chainCode: Sized.Strict[Bytes, ExtendedEd25519.ChainCodeLength]
  ) extends VerificationKey

  case class Vrf(ed25519: Ed25519) extends VerificationKey

  /**
   * @param bytes Merkle Root
   */
  case class Kes(bytes: Sized.Strict[Bytes, Kes.Length]) extends VerificationKey

  object Ed25519 {
    type Length = Lengths.`32`.type
  }

  object ExtendedEd25519 {
    type Length = Lengths.`32`.type
    type ChainCodeLength = Lengths.`32`.type
  }

  object Curve25519 {
    type Length = Lengths.`32`.type
  }

  object Kes {
    type Length = Lengths.`32`.type
  }
}
