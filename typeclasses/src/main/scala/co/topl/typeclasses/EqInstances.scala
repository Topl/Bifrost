package co.topl.typeclasses

import cats.Eq
import cats.implicits._
import co.topl.crypto.mnemonic.Entropy
import co.topl.models._
import co.topl.models.utility.Sized
import co.topl.models.utility.StringDataTypes.Latin1Data

trait EqInstances {

  implicit def arrayEq[T: Eq]: Eq[Array[T]] =
    (a, b) => a.length == b.length && a.zip(b).forall { case (a1, b1) => a1 === b1 }

  implicit val bytesEq: Eq[Bytes] =
    (a, b) => a === b

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

  implicit val blockV2Eq: Eq[BlockV2] =
    Eq.fromUniversalEquals

  implicit val blockHeaderV2Eq: Eq[BlockHeaderV2] =
    Eq.fromUniversalEquals

  implicit val slotIdEq: Eq[SlotId] =
    (a, b) => a.slot === b.slot && a.blockId === b.blockId

  implicit val entropyEq: Eq[Entropy] =
    (a, b) => a.value === b.value

  implicit val curve25519VerificationKey: Eq[VerificationKeys.Curve25519] =
    (a, b) => a.bytes === b.bytes

  implicit val curve25519Signature: Eq[Proofs.Knowledge.Curve25519] =
    (a, b) => a.bytes === b.bytes

  implicit val rhoEq: Eq[Rho] =
    (a, b) => a.sizedBytes === b.sizedBytes

  implicit val slotDataEq: Eq[SlotData] =
    (a, b) =>
      a.slotId === b.slotId &&
      a.parentSlotId === b.parentSlotId &&
      a.rho === b.rho &&
      a.eta === b.eta &&
      a.height === b.height

  implicit val assetCodeEq: Eq[Box.Values.Asset.Code] =
    (a, b) =>
      a.version === b.version &&
      a.issuer === b.issuer &&
      a.shortName === b.shortName
}
