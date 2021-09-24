package co.topl.models

import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, Sized}

// id = hash(blockBytes)
case class BlockV1(
  parentId:     TypedIdentifier,
  timestamp:    Timestamp,
  generatorBox: Box[Box.Values.Arbit],
  publicKey:    Bytes,
  signature:    Bytes,
  height:       Long,
  difficulty:   Long,
  txRoot:       TxRoot,
  bloomFilter:  BloomFilter,
  transactions: Seq[Transaction]
)

// id = hash(headerBytes) INCLUDING kesCertificate proofs
case class BlockHeaderV2(
  parentHeaderId:           TypedIdentifier,
  parentSlot:               Slot,
  txRoot:                   TxRoot,
  bloomFilter:              BloomFilter,
  timestamp:                Timestamp,
  height:                   Long,
  slot:                     Slot,
  eligibibilityCertificate: EligibilityCertificate,
  operationalCertificate:   OperationalCertificate,
  // TODO: Discussion on mint signatures
  metadata: Option[Sized.Max[Latin1Data, Lengths.`32`.type]],
  address:  TaktikosAddress
) {
  def parentSlotId: SlotId = (parentSlot, parentHeaderId)
}

object BlockHeaderV2 {

  case class Unsigned(
    parentHeaderId:           TypedIdentifier,
    parentSlot:               Slot,
    txRoot:                   TxRoot,
    bloomFilter:              BloomFilter,
    timestamp:                Timestamp,
    height:                   Long,
    slot:                     Slot,
    eligibibilityCertificate: EligibilityCertificate,
    metadata:                 Option[Sized.Max[Latin1Data, Lengths.`32`.type]],
    address:                  TaktikosAddress
  )
}

// id = hash(headerId, txRoot)
case class BlockBodyV2(
  headerId:     TypedIdentifier,
  transactions: Seq[Transaction]
)

// This is a synthetic type, and is not "identifiable"
case class BlockV2(headerV2: BlockHeaderV2, blockBodyV2: BlockBodyV2)

object BlockV2 {

  case class Unsigned(
    unsignedHeader: BlockHeaderV2.Unsigned,
    transactions:   Seq[Transaction]
  )
}
