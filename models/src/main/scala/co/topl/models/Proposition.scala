package co.topl.models

import scala.collection.immutable.SortedSet

sealed trait Proposition

object Propositions {

  case class PublicKeyCurve25519(bytes: Sized.Strict[Bytes, Curve25519.PublicKeyLength]) extends Proposition

  case class ThresholdCurve25519(threshold: Int, propositions: SortedSet[PublicKeyCurve25519]) extends Proposition

  case class PublicKeyEd25519(bytes: Sized.Strict[Bytes, Ed25519.PublicKeyLength]) extends Proposition

  case class ThresholdEd25519(threshold: Int, propositions: SortedSet[PublicKeyEd25519]) extends Proposition

}

sealed trait Secret

object Secrets {

  case class PrivateKeyCurve25519(
    privateKey: Sized.Strict[Bytes, Curve25519.PrivateKeyLength],
    publicKey:  Sized.Strict[Bytes, Curve25519.PublicKeyLength]
  ) extends Secret

  case class PrivateKeyEd25519(
    privateKey: Sized.Strict[Bytes, Ed25519.PrivateKeyLength],
    publicKey:  Sized.Strict[Bytes, Ed25519.PublicKeyLength]
  ) extends Secret
}

sealed trait Proof

object Proofs {

  case class SignatureCurve25519(bytes: Option[Sized.Strict[Bytes, Curve25519.PrivateKeyLength]]) extends Proof

  case class ThresholdSignatureCurve25519(signatures: Set[SignatureCurve25519]) extends Proof

  case class SignatureEd25519(bytes: Option[Sized.Strict[Bytes, Ed25519.PrivateKeyLength]]) extends Proof

}

object Curve25519 {
  type PrivateKeyLength = Lengths.`64`.type
  type PublicKeyLength = Lengths.`32`.type
}

object Ed25519 {
  type PrivateKeyLength = Lengths.`64`.type
  type PublicKeyLength = Lengths.`32`.type
}
