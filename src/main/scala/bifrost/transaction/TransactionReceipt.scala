package bifrost.transaction

import io.circe.Json
import io.circe.syntax._
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.crypto.encode.Base58

/**
  * Created by cykoz on 6/8/2017.
  */

case class TransactionReceipt(tx: BifrostTransaction, blockHash: Array[Byte]) extends NodeViewModifier {
  override type M = TransactionReceipt
  override val id = tx.id
  
  override val modifierTypeId: ModifierTypeId = 5: Byte
  override val serializer = tx.serializer.asInstanceOf[Serializer[this.M]]
  override val json: Json = tx.json.asObject.get.add("blockHash", Base58.encode(blockHash).asJson).asJson
}