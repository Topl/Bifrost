package co.topl.models

import cats.data.Chain
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, Sized}

// id = hash(headerBytes) INCLUDING kesCertificate proofs
case class BlockHeader(
  parentHeaderId:         TypedIdentifier,
  parentSlot:             Slot,
  txRoot:                 TxRoot,
  bloomFilter:            BloomFilter,
  timestamp:              Timestamp,
  height:                 Long,
  slot:                   Slot,
  eligibilityCertificate: EligibilityCertificate,
  operationalCertificate: OperationalCertificate,
  // TODO: Discussion on mint signatures
  metadata: Option[BlockHeader.Metadata],
  address:  StakingAddresses.Operator
) {
  def parentSlotId: SlotId = SlotId(parentSlot, parentHeaderId)
}

object BlockHeader {

  type Metadata = Sized.Max[Latin1Data, Lengths.`32`.type]

  case class Unsigned(
    parentHeaderId:                TypedIdentifier,
    parentSlot:                    Slot,
    txRoot:                        TxRoot,
    bloomFilter:                   BloomFilter,
    timestamp:                     Timestamp,
    height:                        Long,
    slot:                          Slot,
    eligibilityCertificate:        EligibilityCertificate,
    partialOperationalCertificate: Unsigned.PartialOperationalCertificate,
    metadata:                      Option[Sized.Max[Latin1Data, Lengths.`32`.type]],
    address:                       StakingAddresses.Operator
  )

  object Unsigned {

    case class PartialOperationalCertificate(
      parentVK:        VerificationKeys.KesProduct,
      parentSignature: Proofs.Knowledge.KesProduct,
      childVK:         VerificationKeys.Ed25519
    )
  }
}

// This is a synthetic type, and is not "identifiable"
case class BlockOld(header: BlockHeader, body: BlockBody) // TODO Remove
case class Block(header: co.topl.consensus.models.BlockHeader, body: BlockBody)

object BlockBody {
  type Full = Chain[Transaction]
}

object Block {

  case class Unsigned(
    unsignedHeader: BlockHeader.Unsigned,
    body:           BlockBody
  )

  case class Full(header: BlockHeader, transactions: BlockBody.Full)
}
