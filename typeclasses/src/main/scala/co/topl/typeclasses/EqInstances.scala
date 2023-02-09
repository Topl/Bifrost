package co.topl.typeclasses

import cats.Eq
import cats.implicits._
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.{models => legacyModels}
import legacyModels._
import legacyModels.utility.Sized
import legacyModels.utility.StringDataTypes.Latin1Data
import co.topl.consensus.{models => consensusModels}
import com.google.protobuf.ByteString

trait EqInstances {

  implicit def arrayEq[T: Eq]: Eq[Array[T]] =
    (a, b) => a.length == b.length && a.zip(b).forall { case (a1, b1) => a1 === b1 }

  implicit val bytesEq: Eq[Bytes] =
    (a, b) => a === b

  implicit val bytesStringEq: Eq[ByteString] = Eq.fromUniversalEquals

  implicit val typedBytesEq: Eq[TypedBytes] =
    (a, b) => a.allBytes === b.allBytes

  implicit def sizedMaxEq[T: Eq, L]: Eq[Sized.Max[T, L]] =
    (a, b) => a.data === b.data

  implicit def sizedStrictEq[T: Eq, L]: Eq[Sized.Strict[T, L]] =
    (a, b) => a.data === b.data

  implicit val latin1DataEq: Eq[Latin1Data] =
    (a, b) => a.value === b.value

  implicit val typedEvidenceEq: Eq[TypedEvidence] =
    (a, b) => a.typePrefix === b.typePrefix && a.evidence === b.evidence

  implicit val spendingAddressEq: Eq[SpendingAddress] =
    (a, b) => a.typedEvidence === b.typedEvidence

  implicit val blockEq: Eq[Block] =
    Eq.fromUniversalEquals

  implicit val blockHeaderEq: Eq[BlockHeader] =
    Eq.fromUniversalEquals

  // TODO Remove after full model replacement
  implicit val slotIdEq: Eq[SlotId] =
    (a, b) => a.slot === b.slot && a.blockId === b.blockId

  implicit val consensusBlockIdEq: Eq[consensusModels.BlockId] =
    (a, b) => a.value === b.value

  implicit val consensusSlotIdEq: Eq[consensusModels.SlotId] =
    (a, b) => a.slot === b.slot && a.blockId === b.blockId

  implicit val entropyEq: Eq[Entropy] =
    (a, b) => a.value === b.value

  implicit val curve25519VerificationKey: Eq[VerificationKeys.Curve25519] =
    (a, b) => a.bytes === b.bytes

  implicit val curve25519Signature: Eq[Proofs.Knowledge.Curve25519] =
    (a, b) => a.bytes === b.bytes

  implicit val rhoEq: Eq[Rho] =
    (a, b) => a.sizedBytes === b.sizedBytes

  // TODO Remove after full model replacement
  implicit val slotDataEq: Eq[SlotDataLegacy] =
    (a, b) =>
      a.slotId === b.slotId &&
      a.parentSlotId === b.parentSlotId &&
      a.rho === b.rho &&
      a.eta === b.eta &&
      a.height === b.height

  implicit val consensusSlotDataEq: Eq[consensusModels.SlotData] =
    (a, b) =>
      a.slotId === b.slotId &&
      a.parentSlotId === b.parentSlotId &&
      a.rho === b.rho &&
      a.eta === b.eta &&
      a.height === b.height

  implicit val assetCodeEq: Eq[Box.Values.AssetV1.Code] =
    (a, b) =>
      a.issuer === b.issuer &&
      a.shortName === b.shortName

  implicit val ed25519ProofEq: Eq[Proofs.Knowledge.Ed25519] =
    (a, b) => a.bytes === b.bytes

  implicit val ed25519VKEq: Eq[VerificationKeys.Ed25519] =
    (a, b) => a.bytes === b.bytes

  implicit val kesSumProofEq: Eq[Proofs.Knowledge.KesSum] =
    (a, b) => a.signature === b.signature && a.verificationKey === b.verificationKey && a.witness === b.witness

  implicit val kesProductProofEq: Eq[Proofs.Knowledge.KesProduct] =
    (a, b) =>
      a.superSignature === b.superSignature &&
      a.subSignature === b.subSignature &&
      a.subRoot === b.subRoot

  implicit val emptyBoxValueEq: Eq[Box.Values.Empty.type] =
    Eq.allEqual

  implicit val polyBoxValueEq: Eq[Box.Values.Poly] =
    (a, b) => a.quantity === b.quantity

  implicit val arbitBoxValueEq: Eq[Box.Values.Arbit] =
    (a, b) => a.quantity === b.quantity

  implicit val assetBoxValueEq: Eq[Box.Values.AssetV1] =
    (a, b) =>
      a.quantity === b.quantity &&
      a.assetCode === b.assetCode &&
      a.securityRoot === b.securityRoot &&
      a.metadata === b.metadata

  implicit val operatorRegistrationBoxValueEq: Eq[Box.Values.Registrations.Operator] =
    (a, b) => a.vrfCommitment === b.vrfCommitment

  implicit val boxValueEq: Eq[Box.Value] = {
    case (a: Box.Values.Empty.type, b: Box.Values.Empty.type) => emptyBoxValueEq.eqv(a, b)
    case (a: Box.Values.Poly, b: Box.Values.Poly)             => polyBoxValueEq.eqv(a, b)
    case (a: Box.Values.Arbit, b: Box.Values.Arbit)           => arbitBoxValueEq.eqv(a, b)
    case (a: Box.Values.AssetV1, b: Box.Values.AssetV1)       => assetBoxValueEq.eqv(a, b)
    case (a: Box.Values.Registrations.Operator, b: Box.Values.Registrations.Operator) =>
      operatorRegistrationBoxValueEq.eqv(a, b)
    case _ => false
  }
}
