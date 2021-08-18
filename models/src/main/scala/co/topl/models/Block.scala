package co.topl.models

sealed abstract class Block

case class BlockV1(
  parentId:     TypedIdentifier,
  timestamp:    Timestamp,
  generatorBox: ArbitBox,
  publicKey:    Bytes,
  signature:    Bytes,
  height:       Long,
  difficulty:   Long,
  transactions: Seq[Transaction]
) extends Block

case class BlockV2(
  parentId:       TypedIdentifier,
  timestamp:      Timestamp,
  height:         Long,
  transactions:   Seq[Transaction],
  slot:           Long,
  vrfCertificate: VrfCertificate,
  kesCertificate: KesCertificate
) extends Block
