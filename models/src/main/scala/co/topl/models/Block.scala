package co.topl.models

import co.topl.models.utility.{Lengths, Sized}
import co.topl.models.utility.StringDataTypes.Latin1Data

// id = hash(blockBytes)
case class BlockV1(
  parentId:     TypedIdentifier,
  timestamp:    Timestamp,
  generatorBox: Box[Box.Values.Arbit],
  publicKey:    Bytes,
  signature:    Bytes,
  height:       Long,
  difficulty:   Long,
  txRoot:       Sized.Strict[Bytes, Lengths.`32`.type],
  bloomFilter:  Sized.Strict[Bytes, Lengths.`256`.type],
  transactions: Seq[Transaction]
)

// id = hash(headerBytes)
case class BlockHeaderV2(
  parentHeaderId:    TypedIdentifier,
  txRoot:            TxRoot,
  bloomFilter:       BloomFilter,
  timestamp:         Timestamp,
  height:            Long,
  slot:              Slot,
  vrfCertificate:    VrfCertificate,
  kesCertificate:    KesCertificate,
  thresholdEvidence: Evidence,
  // TODO: Discussion on mint signatures
  metadata: Option[Sized.Max[Latin1Data, Lengths.`32`.type]],
  address:  TaktikosAddress
)

// id = hash(bodyBytes)
case class BlockBodyV2(
  headerId:     TypedIdentifier,
  transactions: Seq[Transaction]
)

// TODO: ID references, block bodies <-> block headers
// This is a synthetic type, and is not "identifiable"
case class BlockV2(headerV2: BlockHeaderV2, blockBodyV2: BlockBodyV2)
