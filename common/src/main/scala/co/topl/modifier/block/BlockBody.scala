package co.topl.modifier.block

import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.PersistentNodeViewModifier.PNVMVersion
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.utils.NetworkType.NetworkPrefix
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import supertagged.@@

case class BlockBody(id: ModifierId, parentId: ModifierId, transactions: Seq[Transaction.TX], version: PNVMVersion)
    extends TransactionCarryingPersistentNodeViewModifier[Transaction.TX] {

  override lazy val modifierTypeId: ModifierTypeId = BlockBody.modifierTypeId

}

object BlockBody {

  val modifierTypeId: Byte @@ NodeViewModifier.ModifierTypeId.Tag = ModifierTypeId @@ (5: Byte)

  implicit val jsonEncoder: Encoder[BlockBody] = { b: BlockBody =>
    Map(
      "id"       -> b.id.toString.asJson,
      "parentId" -> b.parentId.toString.asJson,
      "txs"      -> b.transactions.asJson,
      "version"  -> b.version.asJson
    ).asJson
  }

  implicit def jsonDecoder(implicit networkPrefix: NetworkPrefix): Decoder[BlockBody] = (c: HCursor) =>
    for {
      id       <- c.downField("id").as[ModifierId]
      parentId <- c.downField("parentId").as[ModifierId]
      txsSeq   <- c.downField("txs").as[Seq[Transaction.TX]]
      version  <- c.downField("version").as[PNVMVersion]
    } yield BlockBody(id, parentId, txsSeq, version)
}
