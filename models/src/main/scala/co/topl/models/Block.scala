package co.topl.models

import cats.data.Chain
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, ReplaceModelUtil, Sized}
import co.topl.{models => legacyModels}
import legacyModels.SlotId
import co.topl.consensus.models._
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
  eligibilityCertificate: legacyModels.EligibilityCertificate,
  operationalCertificate: legacyModels.OperationalCertificate,
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
    eligibilityCertificate:        legacyModels.EligibilityCertificate,
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
    parentHeaderId:                BlockId,
    parentSlot:                    Slot,
    txRoot:                        ByteString,
    bloomFilter:                   ByteString,
    timestamp:                     Timestamp,
    height:                        Long,
    slot:                          Slot,
    eligibilityCertificate:        EligibilityCertificate,
    partialOperationalCertificate: UnsignedConsensus.PartialOperationalCertificate,
    metadata:                      ByteString,
    address:                       ByteString
  )

  object UnsignedConsensus { // TODO rename Unsigned

    case class PartialOperationalCertificate(
      parentVK:        VerificationKeyKesProduct,
      parentSignature: SignatureKesProduct,
      childVK:         VerificationKeyEd25519
    )
  }
}

// This is a synthetic type, and is not "identifiable"
// TODO: Remove class when: https://github.com/Topl/protobuf-specs/pull/37
case class Block(header: co.topl.consensus.models.BlockHeader, body: co.topl.node.models.BlockBody)

object BlockBody {
  // TODO: current com.topl.model, we should replce for use new ProtoModel: co.topl.proto.models;, wich will eventually chain to Brambl's IoTransaction
  type Full = Chain[Transaction]
}

object Block {

  case class UnsignedLegacy(
    unsignedHeader: BlockHeader.Unsigned,
    body:           BlockBody
  )

  case class Unsigned( // TODO ask if we need a new protobuf-spec for BlockUnsigned
    unsignedHeader: BlockHeader.UnsignedConsensus,
    body:           co.topl.node.models.BlockBody
  )

  // TODO remove it after switch protobuf-spsc models
  case class Full(header: BlockHeader, transactions: BlockBody.Full) {

    // intermediate model to switch protobuf-spsc models, remove after that, todo move more functions to ReplaceModelUtil
    def toFullConsensus: FullConsensus = FullConsensus(ReplaceModelUtil.consensusHeader(header), transactions)
  }
  case class FullConsensus(header: co.topl.consensus.models.BlockHeader, transactions: BlockBody.Full)
}
