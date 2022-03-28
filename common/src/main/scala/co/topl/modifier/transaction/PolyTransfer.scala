package co.topl.modifier.transaction

import co.topl.attestation._
import co.topl.modifier.box._
import co.topl.modifier.transaction.Transaction.TxType
import co.topl.modifier.transaction.TransferTransaction.BoxParams
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.utils.{Identifiable, Identifier, Int128}

import scala.collection.immutable.ListMap

case class PolyTransfer[
  P <: Proposition: EvidenceProducer: Identifiable
](
  override val from:        IndexedSeq[(Address, Box.Nonce)],
  override val to:          IndexedSeq[(Address, SimpleValue)],
  override val attestation: ListMap[P, Proof[P]],
  override val fee:         Int128,
  override val timestamp:   Long,
  override val data:        Option[Latin1Data] = None,
  override val minting:     Boolean
) extends TransferTransaction[SimpleValue, P](from, to, attestation, fee, timestamp, data, minting) {

  override val coinOutput: Iterable[PolyBox] =
    coinOutputParams.map { case BoxParams(evi, nonce, value) =>
      PolyBox(evi, nonce, value)
    }

  override val newBoxes: Iterable[TokenBox[SimpleValue]] = {
    // this only creates an output if the value of the output boxes is non-zero
    val recipientCoinOutput: Iterable[PolyBox] = coinOutput.filter(_.value.quantity > 0)
    val hasRecipientOutput: Boolean = recipientCoinOutput.nonEmpty
    val hasFeeChangeOutput: Boolean = feeChangeOutput.value.quantity > 0

    (hasRecipientOutput, hasFeeChangeOutput) match {
      case (false, false) => Iterable()
      case (false, true)  => Iterable(feeChangeOutput) // JAA - only possible because this is Poly TX
      case (true, false)  => recipientCoinOutput
      case (true, true)   => Iterable(feeChangeOutput) ++ recipientCoinOutput
    }
  }
}

object PolyTransfer {
  val typePrefix: TxType = 2: Byte
  val typeString: String = "PolyTransfer"

  implicit def identifier[P <: Proposition]: Identifiable[PolyTransfer[P]] = Identifiable.instance { () =>
    Identifier(typeString, typePrefix)
  }
}
