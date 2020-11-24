package co.topl.modifier.block

import co.topl.attestation.{PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.crypto.Digest32Ops
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.Block.{BlockId, Timestamp, Version}
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.state.box.ArbitBox
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import scorex.crypto.hash.Digest32
import supertagged.@@

case class BlockHeader( id          : BlockId,
                        parentId    : BlockId,
                        timestamp   : Timestamp,
                        forgerBox   : ArbitBox,
                        publicKey   : PublicKeyPropositionCurve25519,
                        signature   : SignatureCurve25519,
                        txRoot      : Digest32,
                        bloomFilter : BloomFilter,
                        version     : Version
                      ) extends PersistentNodeViewModifier {

  override lazy val modifierTypeId: ModifierTypeId = BlockHeader.modifierTypeId

}

object BlockHeader {
  val modifierTypeId: Byte @@ NodeViewModifier.ModifierTypeId.Tag = ModifierTypeId @@ (5: Byte)


  implicit val jsonEncoder: Encoder[BlockHeader] = { bh: BlockHeader ⇒
    Map(
      "id" -> bh.id.toString.asJson,
      "parentId" -> bh.parentId.toString.asJson,
      "timestamp" -> bh.timestamp.asJson,
      "generatorBox" -> bh.forgerBox.asJson,
      "publicKey" -> bh.publicKey.asJson,
      "signature" -> bh.signature.asJson,
      "txRoot" -> bh.txRoot.asJson(Digest32Ops.jsonEncoder),
      "bloomFilter" -> bh.bloomFilter.asJson,
      "version" -> bh.version.asJson,
    ).asJson
  }

  implicit val jsonDecoder: Decoder[Block] = (c: HCursor) =>
    for {
      parentId <- c.downField("parentId").as[ModifierId]
      timestamp <- c.downField("timestamp").as[Timestamp]
      generatorBox <- c.downField("generatorBox").as[ArbitBox]
      publicKey <- c.downField("publicKey").as[PublicKeyPropositionCurve25519]
      signature <- c.downField("signature").as[SignatureCurve25519]
      txsSeq <- c.downField("txs").as[Seq[Transaction.TX]]
      version <- c.downField("version").as[Byte]
    } yield {
      Block(parentId, timestamp, generatorBox, publicKey, signature, txsSeq, version)
    }
}
