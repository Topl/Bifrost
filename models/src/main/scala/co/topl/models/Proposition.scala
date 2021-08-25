package co.topl.models

import co.topl.models.utility.{Lengths, Sized}

import scala.collection.immutable.SortedSet

sealed trait Proposition

object Propositions {

  case class PublicKeyCurve25519(key: PublicKeys.Curve25519) extends Proposition

  case class ThresholdCurve25519(threshold: Int, propositions: SortedSet[PublicKeys.Curve25519]) extends Proposition

  case class PublicKeyEd25519(key: PublicKeys.Ed25519) extends Proposition

  case class ThresholdEd25519(threshold: Int, propositions: SortedSet[PublicKeys.Ed25519]) extends Proposition

  case class Existence() extends Proposition

}

// TODO: Secrets for payment vs. staking
sealed trait Secret

object Secrets {

  case class Curve25519(
    privateKey: PrivateKeys.Curve25519,
    publicKey:  PublicKeys.Curve25519 // TODO: PublicKeyDerivable[Secret]
  ) extends Secret

  case class Ed25519(
    privateKey: PrivateKeys.Ed25519,
    publicKey:  PublicKeys.Ed25519 // TODO: PublicKeyDerivable[Secret]
  ) extends Secret

  case class Vrf(
    privateKey: PrivateKeys.Vrf,
    publicKey:  PublicKeys.Vrf // TODO: PublicKeyDerivable[Secret]
  ) extends Secret

  case class Kes(
    privateKey: PrivateKeys.Kes,
    publicKey:  PublicKeys.Kes // TODO: PublicKeyDerivable[Secret]
  ) extends Secret

}

sealed trait Proof

object Proofs {

  case class SignatureCurve25519(bytes: Option[Sized.Strict[Bytes, PrivateKeys.Curve25519.Length]]) extends Proof

  case class ThresholdSignatureCurve25519(signatures: Set[SignatureCurve25519]) extends Proof

  case class SignatureEd25519(bytes: Option[Sized.Strict[Bytes, PrivateKeys.Ed25519.Length]]) extends Proof

  case class ThresholdSignatureEd25519(signatures: Set[SignatureEd25519]) extends Proof

  case class Existence(id: TypedIdentifier) extends Proof

  case class Nonce(bytes: Sized.Strict[Bytes, Lengths.`64`.type]) extends Proof

  case class VrfTest(bytes: Sized.Strict[Bytes, Lengths.`80`.type]) extends Proof

  case class Kes(bytes: Sized.Strict[Bytes, Lengths.`64`.type]) extends Proof

  case class Block(bytes: Sized.Strict[Bytes, Lengths.`1440`.type]) extends Proof

}

sealed trait PublicKey

object PublicKeys {
  case class Curve25519(bytes: Sized.Strict[Bytes, Curve25519.Length]) extends PublicKey
  case class Ed25519(bytes: Sized.Strict[Bytes, Ed25519.Length]) extends PublicKey
  case class Vrf(ed25519: Ed25519) extends PublicKey
  case class Kes(bytes: Sized.Strict[Bytes, Kes.Length], slot: Slot) extends PublicKey

  object Ed25519 {
    type Length = Lengths.`32`.type
  }

  object Curve25519 {
    type Length = Lengths.`32`.type
  }

  object Kes {
    type Length = Lengths.`32`.type
  }
}

sealed trait PrivateKey

object PrivateKeys {
  case class Curve25519(bytes: Sized.Strict[Bytes, Curve25519.Length]) extends PrivateKey
  case class Ed25519(bytes: Sized.Strict[Bytes, Ed25519.Length]) extends PrivateKey
  case class Vrf(ed25519: Ed25519) extends PrivateKey
  case class Kes(bytes: Sized.Strict[Bytes, Kes.Length]) extends PrivateKey

  object Ed25519 {
    type Length = Lengths.`32`.type
  }

  object Curve25519 {
    type Length = Lengths.`32`.type
  }

  object Kes {
    type Length = Lengths.`32`.type
  }
}
