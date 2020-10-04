package co.topl.modifier.transaction

import co.topl.crypto.{PrivateKey25519, Signature25519}
import co.topl.nodeView.NodeViewModifier.ModifierTypeId
import co.topl.nodeView.state.box.Box
import co.topl.nodeView.state.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import com.google.common.primitives.Longs
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import scorex.crypto.encode.Base58
import supertagged.@@

trait Transaction extends BoxTransaction[ProofOfKnowledgeProposition[PrivateKey25519], Any, Box] {
  lazy val bloomTopics: Option[IndexedSeq[Array[Byte]]] = None

  override val modifierTypeId: ModifierTypeId = Transaction.modifierTypeId

  val boxIdsToOpen: IndexedSeq[Array[Byte]]
}

object Transaction {
  type Nonce = Long
  type Value = Long

  val modifierTypeId: Byte @@ ModifierTypeId.Tag = ModifierTypeId @@ (2: Byte)

  def stringToPubKey ( rawString: String ): PublicKey25519Proposition =
    PublicKey25519Proposition(Base58.decode(rawString).get)

  def stringToSignature ( rawString: String ): Signature25519 = Signature25519(Base58.decode(rawString).get)

  def nonceFromDigest ( digest: Array[Byte] ): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  //implicit val jsonEncoder: Encoder[Transaction] = { tx: Transaction => tx.asJson }

  // fixme: JAA - not lear to me how to do the decoder so skipping for the moment
//  implicit val jsonDecoder: Decoder[_ <: Transaction] = { c: HCursor =>
//    for {
//      txType <- c.downField("txType").as[String]
//    } yield {
//      txType match {
//        case "CodeCreation"           => CodeCreation.jsonDecoder
//        case "ProgramCreation"        => ProgramCreation.jsonDecoder
//        case "ProgramMethodExecution" => ProgramMethodExecution.jsonDecoder
//        case "ProgramTransfer"        => ProgramTransfer.jsonDecoder
//        case "PolyTransfer"           => PolyTransfer.jsonDecoder
//        case "ArbitTransfer"          => ArbitTransfer.jsonDecoder
//        case "AssetTransfer"          => AssetTransfer.jsonDecoder
//        case "AssetCreation"          => AssetCreation.jsonDecoder
//        case "Coinbase"               => Coinbase.jsonDecoder
//      }
//    }
//  }
}
