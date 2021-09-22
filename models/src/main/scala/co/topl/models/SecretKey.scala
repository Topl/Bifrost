package co.topl.models

import co.topl.models.utility.{Lengths, Sized}

sealed trait SecretKey

object SecretKeys {
  case class Curve25519(bytes: Sized.Strict[Bytes, Curve25519.Length]) extends SecretKey

  object Curve25519 {
    type Length = Lengths.`32`.type
  }

  case class Ed25519(bytes: Sized.Strict[Bytes, Ed25519.Length]) extends SecretKey

  object Ed25519 {
    type Length = Lengths.`32`.type
  }

  case class ExtendedEd25519(
    leftKey:   Sized.Strict[Bytes, ExtendedEd25519.LeftLength],
    rightKey:  Sized.Strict[Bytes, ExtendedEd25519.RightLength],
    chainCode: Sized.Strict[Bytes, ExtendedEd25519.ChainCodeLength]
  ) extends SecretKey

  object ExtendedEd25519 {
    type LeftLength = Lengths.`32`.type
    type RightLength = Lengths.`32`.type
    type ChainCodeLength = Lengths.`32`.type
  }

  case class Vrf(ed25519: Ed25519) extends SecretKey
  case class SymmetricMMM(data: KeyData, signature: Proofs.Signature.Ed25519) extends SecretKey
  // Unused
  case class AsymmetricMMM(data: KeyData) extends SecretKey
  case class SumMMM(l: Tree[Array[Byte]], offset: Long) extends SecretKey
}
