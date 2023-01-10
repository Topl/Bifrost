package co.topl.models

import cats.data.Chain
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, ReplaceModelUtil, Sized}

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

  case class UnsignedConsensus(
    parentHeaderId:                com.google.protobuf.ByteString,
    parentSlot:                    Slot,
    txRoot:                        com.google.protobuf.ByteString,
    bloomFilter:                   com.google.protobuf.ByteString,
    timestamp:                     Timestamp,
    height:                        Long,
    slot:                          Slot,
    eligibilityCertificate:        Option[co.topl.consensus.models.EligibilityCertificate],
    partialOperationalCertificate: UnsignedConsensus.PartialOperationalCertificate,
    metadata:                      com.google.protobuf.ByteString,
    address:                       com.google.protobuf.ByteString
  )

  object UnsignedConsensus {

    case class PartialOperationalCertificate(
      parentVK:        Option[co.topl.consensus.models.VerificationKeyKesProduct],
      parentSignature: Option[co.topl.consensus.models.SignatureKesProduct],
      childVK:         Option[co.topl.crypto.models.VerificationKeyEd25519]
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

  // TODO remove it after switch protobuf-spsc models
  case class Full(header: BlockHeader, transactions: BlockBody.Full) {

    // intermediate model to switch protobuf-spsc models, remove after that
    def consensusHeader = co.topl.consensus.models.BlockHeader(
      parentHeaderId = com.google.protobuf.ByteString.copyFrom(header.parentHeaderId.dataBytes.toArray),
      parentSlot = header.parentSlot,
      txRoot = com.google.protobuf.ByteString.copyFrom(header.txRoot.data.toArray),
      bloomFilter = com.google.protobuf.ByteString.copyFrom(header.bloomFilter.data.toArray),
      timestamp = header.timestamp,
      height = header.height,
      slot = header.slot,
      eligibilityCertificate = Some(
        co.topl.consensus.models.EligibilityCertificate(
          vrfSig = Some(
            co.topl.consensus.models.SignatureVrfEd25519(
              value =
                com.google.protobuf.ByteString.copyFrom(this.header.eligibilityCertificate.vrfSig.bytes.data.toArray),
              unknownFields = scalapb.UnknownFieldSet.empty
            )
          )
        )
      ),
      operationalCertificate = Some(
        co.topl.consensus.models.OperationalCertificate(
          parentVK = Some(
            co.topl.consensus.models.VerificationKeyKesProduct(
              value = com.google.protobuf.ByteString.copyFrom(this.header.operationalCertificate.parentVK.bytes.data.toArray),
              step = this.header.operationalCertificate.parentVK.step,
              unknownFields = scalapb.UnknownFieldSet.empty
            )
          ),
          parentSignature = Some(ReplaceModelUtil.signatureKesProduct(header.operationalCertificate.parentSignature)),
          childVK = Some(co.topl.crypto.models.VerificationKeyEd25519(
            value = com.google.protobuf.ByteString.copyFrom(header.operationalCertificate.childVK.bytes.data.toArray),
            unknownFields = scalapb.UnknownFieldSet.empty
          )),
          childSignature = Some(co.topl.crypto.models.SignatureEd25519(
            value = com.google.protobuf.ByteString.copyFrom(header.operationalCertificate.childSignature.bytes.data.toArray),
            unknownFields = scalapb.UnknownFieldSet.empty
          )),
          unknownFields = scalapb.UnknownFieldSet.empty
        )
      ),
      metadata = com.google.protobuf.ByteString.copyFrom(header.metadata.map(_.data.bytes).getOrElse(Array.empty)),
      address = com.google.protobuf.ByteString.copyFrom(header.address.vk.bytes.data.toArray),
      unknownFields = scalapb.UnknownFieldSet.empty
    )
    def toFullConsensus: FullConsensus = FullConsensus(consensusHeader, transactions)
  }
  case class FullConsensus(header: co.topl.consensus.models.BlockHeader, transactions: BlockBody.Full)
}
