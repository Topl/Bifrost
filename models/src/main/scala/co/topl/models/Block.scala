package co.topl.models

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

case class BlockV2(
  parentId:       TypedIdentifier,
  timestamp:      Timestamp,
  height:         Long,
  transactions:   Seq[Transaction],
  slot:           Long,
  vrfCertificate: Bytes,
  kesCertificate: Bytes
)
