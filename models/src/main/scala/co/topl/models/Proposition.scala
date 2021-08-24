package co.topl.models

import scala.collection.immutable.SortedSet

sealed trait Proposition

object Propositions {

  case class PublicKeyCurve25519(key: PublicKeys.Curve25519) extends Proposition

  case class ThresholdCurve25519(threshold: Int, propositions: SortedSet[PublicKeys.Curve25519]) extends Proposition

  case class PublicKeyEd25519(key: PublicKeys.Ed25519) extends Proposition

  case class ThresholdEd25519(threshold: Int, propositions: SortedSet[PublicKeys.Ed25519]) extends Proposition

}

sealed trait Secret

object Secrets {

  case class Curve25519(
    privateKey: PrivateKeys.Curve25519,
    publicKey:  PublicKeys.Curve25519
  ) extends Secret

  case class Ed25519(
    privateKey: PrivateKeys.Ed25519,
    publicKey:  PublicKeys.Ed25519
  ) extends Secret
}

sealed trait Proof

object Proofs {

  case class SignatureCurve25519(bytes: Option[Sized.Strict[Bytes, PrivateKeys.Curve25519.Length]]) extends Proof

  case class ThresholdSignatureCurve25519(signatures: Set[SignatureCurve25519]) extends Proof

  case class SignatureEd25519(bytes: Option[Sized.Strict[Bytes, PrivateKeys.Ed25519.Length]]) extends Proof

}

sealed trait PublicKey

object PublicKeys {
  case class Curve25519(bytes: Sized.Strict[Bytes, Curve25519.Length]) extends PublicKey
  case class Ed25519(bytes: Sized.Strict[Bytes, Ed25519.Length]) extends PublicKey

  object Ed25519 {
    type Length = Lengths.`32`.type
  }

  object Curve25519 {
    type Length = Lengths.`32`.type
  }
}

sealed trait PrivateKey

object PrivateKeys {
  case class Curve25519(bytes: Sized.Strict[Bytes, Curve25519.Length]) extends PublicKey
  case class Ed25519(bytes: Sized.Strict[Bytes, Ed25519.Length]) extends PublicKey

  object Ed25519 {
    type Length = Lengths.`32`.type
  }

  object Curve25519 {
    type Length = Lengths.`32`.type
  }
}
