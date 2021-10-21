package co.topl.models

import co.topl.models.utility.{BinaryTree, KesBinaryTree, Lengths, Sized}

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

  case class KesSum(tree: KesBinaryTree, offset: Int) extends SecretKey

  object KesSum {
    ///type Length = ???
  }

  case class KesProduct() extends SecretKey

  object KesProduct {
    ///type Length = ???
  }

}
