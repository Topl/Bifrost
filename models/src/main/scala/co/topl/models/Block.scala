package co.topl.models

import cats.data.Chain
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, ReplaceModelUtil, Sized}
import com.google.protobuf.ByteString

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

  case class Unsigned( // TODO rename to legacyUnsigned
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

  object Unsigned { // TODO rename to legacyUnsigned

    case class PartialOperationalCertificate(
      parentVK:        VerificationKeys.KesProduct,
      parentSignature: Proofs.Knowledge.KesProduct,
      childVK:         VerificationKeys.Ed25519
    )
  }

  case class UnsignedConsensus( // TODO rename Unsigned
    parentHeaderId:                Option[co.topl.consensus.models.BlockId],
    parentSlot:                    Slot,
    txRoot:                        ByteString,
    bloomFilter:                   ByteString,
    timestamp:                     Timestamp,
    height:                        Long,
    slot:                          Slot,
    eligibilityCertificate:        Option[co.topl.consensus.models.EligibilityCertificate],
    partialOperationalCertificate: UnsignedConsensus.PartialOperationalCertificate,
    metadata:                      ByteString,
    address:                       ByteString
  )

  object UnsignedConsensus { // TODO rename Unsigned

    case class PartialOperationalCertificate(
      parentVK:        Option[co.topl.consensus.models.VerificationKeyKesProduct],
      parentSignature: Option[co.topl.consensus.models.SignatureKesProduct],
      childVK:         Option[co.topl.crypto.models.VerificationKeyEd25519]
    )
  }
}

// This is a synthetic type, and is not "identifiable"
case class BlockOld(header: BlockHeader, body: BlockBody) // TODO Remove
case class Block(header: co.topl.consensus.models.BlockHeader, body: co.topl.node.models.BlockBody)

object BlockBody {
  // TODO: current com.topl.model, we should replce for use new ProtoModel: co.topl.proto.models;, wich will eventually chain to Brambl's IoTransaction
  type Full = Chain[Transaction]
}

object Block {

  case class Unsigned(
    unsignedHeader: BlockHeader.Unsigned,
    body:           BlockBody
  )

  // TODO remove it after switch protobuf-spsc models
  case class Full(header: BlockHeader, transactions: BlockBody.Full) {

    // intermediate model to switch protobuf-spsc models, remove after that, todo move more functions to ReplaceModelUtil
    def toFullConsensus: FullConsensus = FullConsensus(ReplaceModelUtil.consensusHeader(header), transactions)
  }
  case class FullConsensus(header: co.topl.consensus.models.BlockHeader, transactions: BlockBody.Full)
}
