package co.topl.storage.blockchain

import co.topl.storage.graph.Decoder._
import co.topl.storage.graph._
import com.orientechnologies.orient.core.metadata.schema.{OClass, OType}

import scala.jdk.CollectionConverters._

object BlockchainGraphSchema {

  val value: GraphSchema = GraphSchema(
    List(
      BlockHeader.nodeSchema,
      BlockBody.nodeSchema,
      Transaction.nodeSchema,
      Box.nodeSchema,
      Account.nodeSchema,
      ChainHead.nodeSchema,
      State.nodeSchema
    ),
    List(
      BlockParent.edgeSchema,
      BodyHeader.edgeSchema,
      TransactionBlock.edgeSchema,
      TransactionBoxCreates.edgeSchema,
      TransactionBoxOpens.edgeSchema,
      CanonicalHead.edgeSchema,
      BoxAccount.edgeSchema,
      BodyState.edgeSchema,
      StateUnopenedBox.edgeSchema
    )
  )
}

case class BlockHeader(
  blockId:     String,
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

  implicit val nodeSchema: NodeSchema[BlockHeader] = NodeSchema[BlockHeader](
    p = List(
      Property("blockId", OType.STRING),
      Property("timestamp", OType.LONG),
      Property("publicKey", OType.STRING),
      Property("signature", OType.STRING),
      Property("height", OType.LONG),
      Property("difficulty", OType.LONG),
      Property("txRoot", OType.STRING),
      Property("bloomFilter", OType.STRING),
      Property("version", OType.BYTE)
    ),
    i = List(
      Index("BlockHeader_idIndex", OClass.INDEX_TYPE.UNIQUE, "blockId"),
      Index("BlockHeader_timestampIndex", OClass.INDEX_TYPE.NOTUNIQUE, "timestamp"),
      Index("BlockHeader_heightIndex", OClass.INDEX_TYPE.NOTUNIQUE, "height"),
      Index("BlockHeader_bloomFilterIndex", OClass.INDEX_TYPE.NOTUNIQUE, "bloomFilter")
    ),
    enc = header =>
      Map(
        "blockId"     -> header.blockId,
        "timestamp"   -> header.timestamp,
        "publicKey"   -> header.publicKey,
        "signature"   -> header.signature,
        "height"      -> header.height,
        "difficulty"  -> header.difficulty,
        "txRoot"      -> header.txRoot,
        "bloomFilter" -> header.bloomFilter,
        "version"     -> header.version
      ),
    dec = decoder =>
      BlockHeader(
        decoder.typed("blockId"),
        decoder.typed("timestamp"),
        decoder.typed("publicKey"),
        decoder.typed("signature"),
        decoder.typed("height"),
        decoder.typed("difficulty"),
        decoder.typed("txRoot"),
        decoder.typed("bloomFilter"),
        decoder.typed("version")
      )
  )

}

case class BlockBody(blockId: String)

object BlockBody {

  implicit val nodeSchema: NodeSchema[BlockBody] = NodeSchema[BlockBody](
    p = List(
      Property("blockId", OType.STRING)
    ),
    i = List(
      Index("BlockBody_idIndex", OClass.INDEX_TYPE.UNIQUE, "blockId")
    ),
    enc = body =>
      Map(
        "blockId" -> body.blockId
      ),
    dec = decoder =>
      BlockBody(
        decoder.typed("blockId")
      )
  )
}

case class Transaction(
  transactionId: String,
  fee:           String,
  timestamp:     Long,
  data:          Option[String],
  minting:       Boolean,
  attestation:   Map[String, String]
)

object Transaction {

  implicit val nodeSchema: NodeSchema[Transaction] = NodeSchema[Transaction](
    p = List(
      Property("transactionId", OType.STRING),
      Property("fee", OType.STRING),
      Property("timestamp", OType.LONG),
      Property("data", OType.STRING),
      Property("minting", OType.BOOLEAN),
      Property("attestation", OType.EMBEDDEDMAP)
    ),
    i = List(
      Index("Transaction_idIndex", OClass.INDEX_TYPE.UNIQUE, "transactionId"),
      Index("Transaction_timestampIndex", OClass.INDEX_TYPE.NOTUNIQUE, "timestamp")
    ),
    enc = transaction =>
      Map(
        "transactionId"                -> transaction.transactionId,
        "fee"                          -> transaction.fee,
        "timestamp"                    -> transaction.timestamp,
        "minting"                      -> transaction.minting,
        "attestation"                  -> transaction.attestation.asJava
      ) ++ transaction.data.map("data" -> _),
    dec = decoder =>
      Transaction(
        decoder.typed("transactionId"),
        decoder.typed("fee"),
        decoder.typed("timestamp"),
        decoder.getTyped("data"),
        decoder.typed("minting"),
        decoder.typed[java.util.Map[String, String]]("attestation").asScala.toMap
      )
  )

}

case class Box(boxId: String, boxType: Byte, value: String, nonce: Long)

object Box {

  implicit val nodeSchema: NodeSchema[Box] = NodeSchema[Box](
    p = List(
      Property("boxId", OType.STRING),
      Property("boxType", OType.BYTE),
      Property("value", OType.STRING),
      Property("nonce", OType.LONG)
    ),
    i = List(
      Index("Box_idIndex", OClass.INDEX_TYPE.UNIQUE, "boxId"),
      Index("Box_boxTypeIndex", OClass.INDEX_TYPE.NOTUNIQUE, "boxType")
    ),
    enc = box =>
      Map(
        "boxId"   -> box.boxId,
        "boxType" -> box.boxType,
        "value"   -> box.value,
        "nonce"   -> box.nonce
      ),
    dec = decoder =>
      Box(
        decoder.typed("boxId"),
        decoder.typed("boxType"),
        decoder.typed("value"),
        decoder.typed("nonce")
      )
  )
}

case class Account(address: String)

object Account {

  implicit val nodeSchema: NodeSchema[Account] = NodeSchema[Account](
    p = List(
      Property("address", OType.STRING)
    ),
    i = List(
      Index("Account_addressIndex", OClass.INDEX_TYPE.UNIQUE, "address")
    ),
    enc = account =>
      Map(
        "address" -> account.address
      ),
    dec = decoder =>
      Account(
        decoder.typed("address")
      )
  )
}

case class ChainHead()

object ChainHead {

  implicit val nodeSchema: NodeSchema[ChainHead] =
    NodeSchema[ChainHead](
      p = Nil,
      i = Nil,
      enc = _ => Map.empty,
      dec = _ => ChainHead()
    )
}

case class State(stateId: String)

object State {

  implicit val nodeSchema: NodeSchema[State] =
    NodeSchema[State](
      p = List(Property("stateId", OType.STRING)),
      i = List(
        Index("State_stateId", OClass.INDEX_TYPE.UNIQUE, "stateId")
      ),
      enc = state => Map("stateId" -> state.stateId),
      dec = decoder => State(decoder.typed("stateId"))
    )
}

case class BlockParent()

object BlockParent {

  implicit val edgeSchema: EdgeSchema[BlockParent, BlockHeader, BlockHeader] =
    EdgeSchema(enc = _ => Map.empty, dec = _ => BlockParent())
}

case class BodyHeader()

object BodyHeader {

  implicit val edgeSchema: EdgeSchema[BodyHeader, BlockBody, BlockHeader] =
    EdgeSchema(enc = _ => Map.empty, dec = _ => BodyHeader())
}

case class TransactionBlock()

object TransactionBlock {

  implicit val edgeSchema: EdgeSchema[TransactionBlock, Transaction, BlockBody] =
    EdgeSchema(enc = _ => Map.empty, dec = _ => TransactionBlock())

}

case class TransactionBoxCreates(minted: Boolean)

object TransactionBoxCreates {

  implicit val edgeSchema: EdgeSchema[TransactionBoxCreates, Transaction, Box] =
    EdgeSchema(
      List(Property("minted", OType.BOOLEAN)),
      enc = v => Map("minted" -> v.minted),
      dec = decoder => TransactionBoxCreates(decoder.typed("minted"))
    )
}

case class TransactionBoxOpens()

object TransactionBoxOpens {

  implicit val edgeSchema: EdgeSchema[TransactionBoxOpens, Transaction, Box] =
    EdgeSchema(
      enc = _ => Map.empty,
      dec = _ => TransactionBoxOpens()
    )
}

case class CanonicalHead()

object CanonicalHead {

  implicit val edgeSchema: EdgeSchema[CanonicalHead, ChainHead, BlockHeader] =
    EdgeSchema(enc = _ => Map.empty, dec = _ => CanonicalHead())

}

case class BoxAccount()

object BoxAccount {

  implicit val edgeSchema: EdgeSchema[BoxAccount, Box, Account] =
    EdgeSchema(enc = _ => Map.empty, dec = _ => BoxAccount())

}

case class BodyState()

object BodyState {

  implicit val edgeSchema: EdgeSchema[BodyState, BlockBody, State] =
    EdgeSchema(enc = _ => Map.empty, dec = _ => BodyState())

}

case class StateUnopenedBox()

object StateUnopenedBox {

  implicit val edgeSchema: EdgeSchema[StateUnopenedBox, State, Box] =
    EdgeSchema(enc = _ => Map.empty, dec = _ => StateUnopenedBox())

}
