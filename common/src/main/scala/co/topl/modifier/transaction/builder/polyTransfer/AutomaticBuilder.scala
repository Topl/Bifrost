package co.topl.modifier.transaction.builder.polyTransfer

import cats.implicits._
import co.topl.attestation.{Address, EvidenceProducer, Proposition}
import co.topl.modifier.BoxReader
import co.topl.modifier.box.{Box, ProgramId, SimpleValue}
import co.topl.modifier.transaction.TransferTransaction.TransferCreationState
import co.topl.modifier.transaction.{PolyTransfer, TransferTransaction}
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.utils.{Identifiable, Int128}

import java.time.Instant
import scala.collection.immutable.ListMap

case class AutomaticBuilder(
  boxReader:     BoxReader[ProgramId, Address],
  toReceive:     IndexedSeq[(Address, SimpleValue)],
  sender:        IndexedSeq[Address],
  changeAddress: Address,
  fee:           Int128,
  data:          Option[Latin1Data]
)

object AutomaticBuilder {

  /** construct input and output box sequence for a transfer transaction */
  private def ioTransfer(
    txInputState:  TransferCreationState,
    toReceive:     IndexedSeq[(Address, SimpleValue)],
    changeAddress: Address,
    fee:           Int128,
    amtToSpend:    Int128
  ): (Int128, IndexedSeq[(Address, Box.Nonce)], IndexedSeq[(Address, SimpleValue)]) = {

    val availableToSpend = txInputState.polyBalance - fee
    val inputs = txInputState.senderBoxes("Poly").map(bxs => (bxs._2, bxs._3.nonce))
    val outputs = (changeAddress, SimpleValue(txInputState.polyBalance - fee - amtToSpend)) +: toReceive
    val filterZeroChange = outputs.filter(_._2.quantity > 0)

    (availableToSpend, inputs, filterZeroChange)
  }

  def buildPolyTransfer[P <: Proposition: EvidenceProducer: Identifiable](
    buildStrategy: AutomaticBuilder
  ): Either[InvalidPolyTransfer, PolyTransfer[P]] =
    TransferTransaction
      .getSenderBoxesAndCheckPolyBalance(
        buildStrategy.boxReader,
        buildStrategy.sender,
        buildStrategy.fee,
        "Polys"
      ) // you always get Polys back
      .toEither
      .leftMap(_ => InsufficientFunds)
      .flatMap { txInputState =>
        // compute the amount of tokens that will be sent to the recipients
        val amtToSpend = buildStrategy.toReceive.map(_._2.quantity).sum

        // create the list of inputs and outputs (senderChangeOut & recipientOut)
        val (availableToSpend, inputs, outputs) =
          ioTransfer(txInputState, buildStrategy.toReceive, buildStrategy.changeAddress, buildStrategy.fee, amtToSpend)
        // ensure there are sufficient funds from the sender boxes to create all outputs

        Either.cond(
          availableToSpend >= amtToSpend,
          PolyTransfer[P](
            inputs,
            outputs,
            ListMap(),
            buildStrategy.fee,
            Instant.now.toEpochMilli,
            buildStrategy.data,
            minting = false
          ),
          InsufficientFunds
        )
      }
}
