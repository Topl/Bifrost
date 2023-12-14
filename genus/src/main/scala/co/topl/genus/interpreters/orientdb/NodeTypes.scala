package co.topl.genus.interpreters.orientdb

import com.orientechnologies.orient.core.metadata.schema.OClass

object NodeTypes {
  case object CanonicalHead

  case class Header(
    id:        String,
    txRoot:    String,
    timestamp: Long,
    height:    Long,
    slot:      Long,
    address:   String
  )

  case class Transaction(
    id:                String,
    creationTimestamp: Long,
    minimumSlot:       Long,
    maximumSlot:       Long,
    data:              Option[String]
  )

  // TODO: Edge to parent TransactionOutput
  case class TransactionInput(
    proposition: String,
    proof:       String
  )

  // TODO: Edge to address.spendingAddress
  // TODO: Edge to address.stakingAddress
  case class TransactionOutput(
    address: String,
    // TODO
//    value:   BoxValue,
    minting: Boolean
  )

}

object NodeSchemas {

  implicit val canonicalHeadNodeSchema: NodeSchema[NodeTypes.CanonicalHead.type] =
    NodeSchema.create("CanonicalHead", GraphDataEncoder[NodeTypes.CanonicalHead.type], _ => NodeTypes.CanonicalHead)

  implicit val headerNodeSchema: NodeSchema[NodeTypes.Header] =
    NodeSchema.create(
      "Header",
      GraphDataEncoder[NodeTypes.Header]
        .withProperty("blockId", _.id, Set(("UniqueBlockId", OClass.INDEX_TYPE.UNIQUE)))
        .withProperty("txRoot", _.txRoot)
        .withProperty[java.lang.Long](
          "timestamp",
          t => Long.box(t.timestamp),
          Set(("TimestampIdx", OClass.INDEX_TYPE.NOTUNIQUE))
        )
        .withProperty[java.lang.Long]("height", _.height, Set(("HeightIdx", OClass.INDEX_TYPE.NOTUNIQUE)))
        .withProperty[java.lang.Long]("slot", _.slot, Set(("SlotIdx", OClass.INDEX_TYPE.NOTUNIQUE)))
        .withProperty("address", _.address, Set(("AddressIdx", OClass.INDEX_TYPE.NOTUNIQUE))),
      v =>
        NodeTypes.Header(
          v("blockId"),
          v("txRoot"),
          v("timestamp"),
          v("height"),
          v("slot"),
          v("address")
        )
    )

  implicit val transactionNodeSchema: NodeSchema[NodeTypes.Transaction] =
    NodeSchema.create(
      "Transaction",
      GraphDataEncoder[NodeTypes.Transaction]
        .withProperty("transactionId", _.id, Set(("UniqueTransactionId", OClass.INDEX_TYPE.UNIQUE)))
        .withProperty[java.lang.Long]("creationTimestamp", _.creationTimestamp)
        .withProperty[java.lang.Long]("minimumSlot", _.minimumSlot)
        .withProperty[java.lang.Long]("maximumSlot", _.maximumSlot)
        .withProperty("data", _.data),
      v =>
        NodeTypes.Transaction(
          v("transactionId"),
          v("creationTimestamp"),
          v("minimumSlot"),
          v("maximumSlot"),
          v("data")
        )
    )

  implicit val transactionInputNodeSchema: NodeSchema[NodeTypes.TransactionInput] =
    NodeSchema.create(
      "TransactionInput",
      GraphDataEncoder[NodeTypes.TransactionInput]
        .withProperty("proposition", _.proposition)
        .withProperty("proof", _.proof),
      v => NodeTypes.TransactionInput(v("proposition"), v("proof"))
    )

  implicit val transactionOutputNodeSchema: NodeSchema[NodeTypes.TransactionOutput] =
    NodeSchema.create(
      "TransactionOutput",
      GraphDataEncoder[NodeTypes.TransactionOutput]
        .withProperty("address", _.address)
        .withProperty[java.lang.Boolean]("minting", _.minting),
      v => NodeTypes.TransactionOutput(v("address"), v("minting"))
    )
}
