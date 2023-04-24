package co.topl.typeclasses

import cats.Eq
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.models._
import co.topl.models.utility.Sized
import co.topl.consensus.models._
import com.google.protobuf.ByteString

trait EqInstances {

  implicit def arrayEq[T: Eq]: Eq[Array[T]] =
    (a, b) => a.length == b.length && a.zip(b).forall { case (a1, b1) => a1 === b1 }

  implicit val bytesStringEq: Eq[ByteString] =
    Eq.fromUniversalEquals

  implicit def sizedMaxEq[T: Eq, L]: Eq[Sized.Max[T, L]] =
    (a, b) => a.data === b.data

  implicit def sizedStrictEq[T: Eq, L]: Eq[Sized.Strict[T, L]] =
    (a, b) => a.data === b.data

  implicit val entropyEq: Eq[Entropy] =
    (a, b) => a.value === b.value

  implicit val rhoEq: Eq[Rho] =
    (a, b) => a.sizedBytes === b.sizedBytes

  implicit val blockIdEq: Eq[BlockId] =
    (a, b) => a.value === b.value

  implicit val transactionIdEq: Eq[TransactionId] =
    (a, b) => a.value === b.value

  implicit val eqSlotData: Eq[SlotData] =
    Eq.fromUniversalEquals
}
