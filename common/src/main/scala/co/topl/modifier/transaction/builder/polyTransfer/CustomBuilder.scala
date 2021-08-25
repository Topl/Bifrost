package co.topl.modifier.transaction.builder.polyTransfer

import cats.implicits._
import co.topl.attestation.{Address, EvidenceProducer, Proposition}
import co.topl.modifier.BoxReader
import co.topl.modifier.box.{Box, ProgramId, SimpleValue}
import co.topl.modifier.transaction.PolyTransfer
import co.topl.modifier.transaction.builder.polyTransfer.PolyTransferBuildStrategy.InvalidPolyTransfer
import co.topl.utils.{Identifiable, Int128}
import co.topl.utils.StringDataTypes.Latin1Data

import scala.collection.immutable.ListMap

case class CustomBuilder(
  fromBoxes:     IndexedSeq[(Address, Box.Nonce)],
  to:            IndexedSeq[(Address, SimpleValue)],
  changeAddress: Address,
  fee:           Int128,
  timestamp:     Long,
  data:          Option[Latin1Data],
  minting:       Boolean
)

object CustomBuilder {

  def buildPolyTransfer[P <: Proposition: EvidenceProducer: Identifiable](
    buildStrategy: CustomBuilder
  ): PolyTransfer[P] =
    PolyTransfer[P](
      buildStrategy.fromBoxes,
      buildStrategy.to,
      ListMap(),
      buildStrategy.fee,
      buildStrategy.timestamp,
      buildStrategy.data,
      buildStrategy.minting
    )
}
