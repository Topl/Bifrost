package co.topl.genus.interpreters.orientdb

object EdgeTypes {
  case object CanonicalHead
  case object HeaderToParentHeader
  case class TransactionToHeader(index: Short)
  case class TransactionToInput(index: Short)
  case class TransactionToOutput(index: Short)
  // TODO: OutputAsInput
  case object InputToOutput
}

object EdgeSchemas {

  implicit val canonicalHeadEdgeSchema
    : EdgeSchema[EdgeTypes.CanonicalHead.type, NodeTypes.CanonicalHead.type, NodeTypes.Header] =
    EdgeSchema.create("CanonicalHeadE", GraphDataEncoder.apply, _ => EdgeTypes.CanonicalHead)

  implicit val headerToParentEdgeSchema
    : EdgeSchema[EdgeTypes.HeaderToParentHeader.type, NodeTypes.Header, NodeTypes.Header] =
    EdgeSchema.create("HeaderToParentHeader", GraphDataEncoder.apply, _ => EdgeTypes.HeaderToParentHeader)

  implicit val transactionToHeaderEdgeSchema
    : EdgeSchema[EdgeTypes.TransactionToHeader, NodeTypes.Transaction, NodeTypes.Header] =
    EdgeSchema.create(
      "TransactionToHeader",
      GraphDataEncoder[EdgeTypes.TransactionToHeader].withProperty[java.lang.Short]("index", _.index),
      v => EdgeTypes.TransactionToHeader(v("index"))
    )

  implicit val transactionToInputEdgeSchema
    : EdgeSchema[EdgeTypes.TransactionToInput, NodeTypes.Transaction, NodeTypes.TransactionInput] =
    EdgeSchema.create(
      "TransactionToInput",
      GraphDataEncoder[EdgeTypes.TransactionToInput].withProperty[java.lang.Short]("index", _.index),
      v => EdgeTypes.TransactionToInput(v("index"))
    )

  implicit val transactionToOutputEdgeSchema
    : EdgeSchema[EdgeTypes.TransactionToOutput, NodeTypes.Transaction, NodeTypes.TransactionOutput] =
    EdgeSchema.create(
      "TransactionToOutput",
      GraphDataEncoder[EdgeTypes.TransactionToOutput].withProperty[java.lang.Short]("index", _.index),
      v => EdgeTypes.TransactionToOutput(v("index"))
    )

  implicit val inputToOutputEdgeSchema
    : EdgeSchema[EdgeTypes.InputToOutput.type, NodeTypes.TransactionInput, NodeTypes.TransactionOutput] =
    EdgeSchema.create("InputToOutput", GraphDataEncoder.apply, _ => EdgeTypes.InputToOutput)

}
