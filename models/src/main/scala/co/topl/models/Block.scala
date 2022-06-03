package co.topl.models

import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, Sized}

// id = hash(blockBytes)
case class BlockV1(
  parentId:     TypedIdentifier,
  timestamp:    Timestamp,
  generatorBox: Box,
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
  parentHeaderId:         TypedIdentifier,
  parentSlot:             Slot, // TODO: Maybe remove
  txRoot:                 TxRoot,
  bloomFilter:            BloomFilter, // TODO: Maybe move to Body
  timestamp:              Timestamp,
  height:                 Long,
  slot:                   Slot,
  eligibilityCertificate: EligibilityCertificate,
  operationalCertificate: OperationalCertificate,
  // TODO: Discussion on mint signatures
  metadata: Option[Sized.Max[Latin1Data, Lengths.`32`.type]],
  address:  StakingAddresses.Operator
) {
  def parentSlotId: SlotId = SlotId(parentSlot, parentHeaderId)
}

object BlockHeaderV2 {

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
case class BlockV2(headerV2: BlockHeaderV2, blockBodyV2: BlockBodyV2)

object BlockV2 {

  case class Unsigned(
    unsignedHeader: BlockHeaderV2.Unsigned,
    body:           BlockBodyV2
  )
}
