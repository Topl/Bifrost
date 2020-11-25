package co.topl.modifier.block

import co.topl.modifier.NodeViewModifier
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.Block.BlockId
import co.topl.modifier.block.PersistentNodeViewModifier.PNVMVersion
import co.topl.modifier.transaction.Transaction
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import supertagged.@@

case class BlockBody(id: BlockId,
                     parentId: BlockId,
                     transactions: Seq[Transaction.TX],
                     version: PNVMVersion
                    ) extends TransactionCarryingPersistentNodeViewModifier[Transaction.TX] {

  override lazy val modifierTypeId: ModifierTypeId = BlockBody.modifierTypeId

}

object BlockBody {

  val modifierTypeId: Byte @@ NodeViewModifier.ModifierTypeId.Tag = ModifierTypeId @@ (5: Byte)

  implicit val jsonEncoder: Encoder[BlockBody] = { b: BlockBody ⇒
    Map(
      "id" -> b.id.toString.asJson,
      "parentId" -> b.parentId.toString.asJson,
      "txs" -> b.transactions.asJson,
      "version" -> b.version.asJson,
    ).asJson
  }

  implicit val jsonDecoder: Decoder[BlockBody] = (c: HCursor) =>
    for {
      id <- c.downField("id").as[BlockId]
      parentId <- c.downField("parentId").as[BlockId]
      txsSeq <- c.downField("txs").as[Seq[Transaction.TX]]
      version <- c.downField("version").as[PNVMVersion]
    } yield {
      BlockBody(id, parentId, txsSeq, version)
    }
}