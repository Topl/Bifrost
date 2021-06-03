package co.topl.storage.graph

import com.orientechnologies.orient.core.metadata.schema.{OClass, OType}

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

object BlockHeader {

  implicit val nodeSchema: NodeSchema[BlockHeader] = new NodeSchema[BlockHeader] {
    override val name: String = "BlockHeader"

    override val properties: Map[String, OType] = Map(
      "blockId"     -> OType.STRING,
      "timestamp"   -> OType.LONG,
      "publicKey"   -> OType.STRING,
      "height"      -> OType.LONG,
      "difficulty"  -> OType.LONG,
      "txRoot"      -> OType.STRING,
      "bloomFilter" -> OType.STRING,
      "version"     -> OType.BYTE
    )

    override val indices: Map[String, OClass.INDEX_TYPE] = Map(
      "BlockHeader_idIndex" -> OClass.INDEX_TYPE.UNIQUE_HASH_INDEX
      "BlockHeader_timestampIndex" -> OClass.INDEX_TYPE.UNIQUE_HASH_INDEX
    )

    override def srcEdges: Set[EdgeSchema[_, _, BlockHeader]] = ???

    override def destEdges: Set[EdgeSchema[_, BlockHeader, _]] = ???
  }
}

case class BlockParent()

object BlockParent {

  implicit val edgeSchema: EdgeSchema[BlockParent, BlockHeader, BlockHeader] =
    new EdgeSchema[BlockParent, BlockHeader, BlockHeader] {
      override val name: String = "BlockParent"

      override val properties: Map[String, OType] = Map.empty

      override def srcSchema: NodeSchema[BlockHeader] = BlockHeader.nodeSchema

      override def destSchema: NodeSchema[BlockHeader] = BlockHeader.nodeSchema
    }
}

case class BlockBody(id: String)

object BlockBody {

  implicit val nodeSchema: NodeSchema[BlockBody] = new NodeSchema[BlockBody] {
    override val name: String = "BlockBody"

    override val properties: Map[String, OType] = Map(
      "blockId" -> OType.STRING
    )

    override def srcEdges: Set[EdgeSchema[_, _, BlockBody]] = ???

    override def destEdges: Set[EdgeSchema[_, BlockBody, _]] = ???
  }
}

case class BodyHeader()

object BodyHeader {

  implicit val edgeSchema: EdgeSchema[BodyHeader, BlockBody, BlockHeader] =
    new EdgeSchema[BodyHeader, BlockBody, BlockHeader] {
      override val name: String = "BodyHeader"

      override val properties: Map[String, OType] = Map.empty

      override def srcSchema: NodeSchema[BlockBody] = BlockBody.nodeSchema

      override def destSchema: NodeSchema[BlockHeader] = BlockHeader.nodeSchema
    }
}

case class Transaction(id: String, fee: String, timestamp: Long, data: Option[String], minting: Boolean)

object Transaction {

  implicit val nodeSchema: NodeSchema[Transaction] = new NodeSchema[Transaction] {
    override val name: String = "Transaction"

    override val properties: Map[String, OType] = Map(
    )

    override def srcEdges: Set[EdgeSchema[_, _, Transaction]] = ???

    override def destEdges: Set[EdgeSchema[_, Transaction, _]] = ???
  }
}

case class Box(id: String, boxType: String, evidence: String, value: String, nonce: Long)

case class Account(address: String)
