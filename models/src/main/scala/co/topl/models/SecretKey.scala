package co.topl.models

import co.topl.models.utility.{Lengths, Sized, Tree}

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
    type Length = Lengths.`96`.type
    type LeftLength = Lengths.`32`.type
    type RightLength = Lengths.`32`.type
    type ChainCodeLength = Lengths.`32`.type
  }

  case class VrfEd25519(bytes: Sized.Strict[Bytes, VrfEd25519.Length]) extends SecretKey

  object VrfEd25519 {
    type Length = Lengths.`32`.type //todo: check this value
  }

  case class KesSum(tree: Tree[Array[Byte]], offset: Long) extends SecretKey

  object KesSum {
    ///type Length = ???
  }

  case class KesSymmetricProduct(
    superScheme:        Tree[Array[Byte]],
    subScheme:          Tree[Array[Byte]],
    subSchemeSignature: Bytes,
    subSchemePublicKey: Bytes,
    subSchemeSeed:      Bytes,
    offset:             Long,
    signature:          Proofs.Signature.Ed25519
  ) extends SecretKey

  object KesSymmetricProduct {
    ///type Length = ???
  }

  case class KesAsymmetricProduct(
    superScheme:        Tree[Array[Byte]],
    subScheme:          Tree[Array[Byte]],
    subSchemeSignature: Bytes,
    subSchemePublicKey: Bytes,
    subSchemeSeed:      Bytes,
    offset:             Long
  ) extends SecretKey

  object KesAsymmetricProduct {
    ///type Length = ???
  }

  case class HdKesSum(tree: Tree[Array[Byte]]) extends SecretKey

  object HdKesSum {
    ///type Length = ???
  }

//  case class SymmetricMMM(data: KeyData, signature: Proofs.Signature.Ed25519) extends SecretKey
//
//  case class AsymmetricMMM(data: KeyData) extends SecretKey
//
//  case class SumMMM(l: Tree[Array[Byte]], offset: Long) extends SecretKey

}
