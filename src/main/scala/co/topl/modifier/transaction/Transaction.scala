package co.topl.modifier.transaction

import co.topl.attestation.proposition.Proposition
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.transaction.serialization.TransactionSerializer
import co.topl.nodeView.state.box.{ Box, BoxId }
import co.topl.utils.serialization.BifrostSerializer
import com.google.common.primitives.Longs
import io.circe.{ Decoder, Encoder, HCursor, Json }
import supertagged.@@

trait Transaction[T, P <: Proposition] extends BoxTransaction[T, P, Box[T]] {

  override type M = Transaction[_, _ <: Proposition]

  override val modifierTypeId: ModifierTypeId = Transaction.modifierTypeId

  override lazy val json: Json = Transaction.jsonEncoder(this)

  override lazy val serializer: BifrostSerializer[Transaction[_, _ <: Proposition]] = TransactionSerializer

  lazy val bloomTopics: Option[IndexedSeq[Array[Byte]]] = None

  val boxIdsToOpen: IndexedSeq[BoxId]
}


object Transaction {

  val modifierTypeId: Byte @@ ModifierTypeId.Tag = ModifierTypeId @@ (2: Byte)

  def nonceFromDigest ( digest: Array[Byte] ): Box.Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  implicit val jsonEncoder: Encoder[Transaction[_, _]] = {
    case tx: CodeCreation           => CodeCreation.jsonEncoder(tx)
    case tx: ProgramCreation        => ProgramCreation.jsonEncoder(tx)
    case tx: ProgramMethodExecution => ProgramMethodExecution.jsonEncoder(tx)
    case tx: ProgramTransfer        => ProgramTransfer.jsonEncoder(tx)
    case tx: PolyTransfer           => PolyTransfer.jsonEncoder(tx)
    case tx: ArbitTransfer          => ArbitTransfer.jsonEncoder(tx)
    case tx: AssetTransfer          => AssetTransfer.jsonEncoder(tx)
    case tx: AssetCreation          => AssetCreation.jsonEncoder(tx)
    case tx: Coinbase               => Coinbase.jsonEncoder(tx)
  }

  implicit val jsonDecoder: Decoder[Transaction[_, _]] = { c: HCursor =>
    c.downField("txType").as[String].map {
      case "CodeCreation"           => CodeCreation.jsonDecoder(c)
      case "ProgramCreation"        => ProgramCreation.jsonDecoder(c)
      case "ProgramMethodExecution" => ProgramMethodExecution.jsonDecoder(c)
      case "ProgramTransfer"        => ProgramTransfer.jsonDecoder(c)
      case "PolyTransfer"           => PolyTransfer.jsonDecoder(c)
      case "ArbitTransfer"          => ArbitTransfer.jsonDecoder(c)
      case "AssetTransfer"          => AssetTransfer.jsonDecoder(c)
      case "AssetCreation"          => AssetCreation.jsonDecoder(c)
      case "Coinbase"               => Coinbase.jsonDecoder(c)
    } match {
      case Right(tx) => tx
      case Left(ex)  => throw ex
    }
  }
}
