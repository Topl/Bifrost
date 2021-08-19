package co.topl.models

// id = hash(blockBytes)
case class BlockV1(
  parentId:     TypedIdentifier,
  timestamp:    Timestamp,
  generatorBox: ArbitBox,
  publicKey:    Bytes,
  signature:    Bytes,
  height:       Long,
  difficulty:   Long,
  transactions: Seq[Transaction]
)

// id = hash(headerBytes)
case class BlockHeaderV2(
  parentHeaderId: TypedIdentifier,
  blockBodyId:    TypedIdentifier,
  timestamp:      Timestamp,
  height:         Long,
  slot:           Slot,
  vrfCertificate: VrfCertificate,
  kesCertificate: KesCertificate
)

// id = hash(bodyBytes)
case class BlockBodyV2(
  parentHeaderId: TypedIdentifier,
  transactions:   Seq[Transaction]
)

// This is a synthetic type, and is not "identifiable"
case class BlockV2(headerV2: BlockHeaderV2, blockBodyV2: BlockBodyV2)
