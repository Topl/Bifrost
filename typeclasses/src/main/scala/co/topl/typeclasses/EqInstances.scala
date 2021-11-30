package co.topl.typeclasses

import cats.Eq
import cats.implicits._
import co.topl.crypto.mnemonic.Entropy
import co.topl.models._
import co.topl.models.utility.Sized

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

  implicit val blockV2Eq: Eq[BlockV2] =
    Eq.fromUniversalEquals

  implicit val blockHeaderV2Eq: Eq[BlockHeaderV2] =
    Eq.fromUniversalEquals

  implicit val slotIdEq: Eq[SlotId] =
    (a, b) => a.slot === b.slot && a.blockId === b.blockId

  implicit val entropyEq: Eq[Entropy] =
    (a, b) => a.value === b.value

}
