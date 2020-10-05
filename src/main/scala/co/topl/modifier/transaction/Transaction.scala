package co.topl.modifier.transaction

import co.topl.crypto.PrivateKey25519
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.nodeView.state.box.proposition.ProofOfKnowledgeProposition
import co.topl.nodeView.state.box.{Box, BoxId}
import com.google.common.primitives.Longs
import io.circe.{Decoder, Encoder, HCursor}
import supertagged.@@

trait Transaction extends BoxTransaction[ProofOfKnowledgeProposition[PrivateKey25519], Any, Box] {

  override val modifierTypeId: ModifierTypeId = Transaction.modifierTypeId

  lazy val bloomTopics: Option[IndexedSeq[Array[Byte]]] = None

  val boxIdsToOpen: IndexedSeq[BoxId]
}

object Transaction {
  type Nonce = Long
  type Value = Long

  val modifierTypeId: Byte @@ ModifierTypeId.Tag = ModifierTypeId @@ (2: Byte)

  def nonceFromDigest ( digest: Array[Byte] ): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  implicit val jsonEncoder: Encoder[Transaction] = { tx: Transaction => tx.json }

  implicit val jsonDecoder: Decoder[Transaction] = { c: HCursor =>
    for {
      txType <- c.downField("txType").as[String]
    } yield {
      txType match {
        case "CodeCreation"           => CodeCreation.jsonDecoder
        case "ProgramCreation"        => ProgramCreation.jsonDecoder
        case "ProgramMethodExecution" => ProgramMethodExecution.jsonDecoder
        case "ProgramTransfer"        => ProgramTransfer.jsonDecoder
        case "PolyTransfer"           => PolyTransfer.jsonDecoder
        case "ArbitTransfer"          => ArbitTransfer.jsonDecoder
        case "AssetTransfer"          => AssetTransfer.jsonDecoder
        case "AssetCreation"          => AssetCreation.jsonDecoder
        case "Coinbase"               => Coinbase.jsonDecoder
      }
    }
  }
}
