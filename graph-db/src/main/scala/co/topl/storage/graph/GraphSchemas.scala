package co.topl.storage.graph

class GraphSchemas {}

case class BlockHeader(
  id:          String,
  timestamp:   Long,
  publicKey:   String,
  signature:   String,
  height:      Long,
  difficulty:  Long,
  txRoot:      String,
  bloomFilter: String,
  version:     Byte
)

case class BlockBody(id: String)

case class Transaction(id: String, fee: String, timestamp: Long, data: Option[String], minting: Boolean)

case class Box(id: String, boxType: String, evidence: String, value: String, nonce: Long)
