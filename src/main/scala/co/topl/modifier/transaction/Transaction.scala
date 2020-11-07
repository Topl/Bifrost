package co.topl.modifier.transaction

import co.topl.attestation.proof.Proof
import co.topl.attestation.proposition.Proposition
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.transaction.serialization.TransactionSerializer
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.nodeView.state.box.{Box, BoxId, GenericBox}
import co.topl.utils.serialization.BifrostSerializer
import com.google.common.primitives.Longs
import io.circe.{Decoder, Encoder, HCursor}
import scorex.crypto.hash.{Blake2b256, Digest32}

abstract class Transaction[T, P <: Proposition, PR <: Proof[P], BX <: GenericBox[T]] extends NodeViewModifier {

  override type M = Transaction[_, _ <: Proposition, _ <: Proof[_], _ <: GenericBox[_]]

  override lazy val id: ModifierId = ModifierId(Blake2b256(messageToSign))

  override lazy val serializer: BifrostSerializer[Transaction[_, _ <: Proposition, _ <: Proof[_], _ <: GenericBox[_]]] =
    TransactionSerializer

  lazy val bloomTopics: Option[IndexedSeq[Array[Byte]]] = None

  lazy val digest: Digest32 = Blake2b256(messageToSign)

  lazy val messageToSign: Array[Byte] =
    transactionName.getBytes() ++
      newBoxes.foldLeft(Array[Byte]())((acc, x) => acc ++ x.bytes) ++
      boxIdsToOpen.foldLeft(Array[Byte]())((acc, x) => acc ++ x.hashBytes) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)

  val modifierTypeId: ModifierTypeId = Transaction.modifierTypeId

  val transactionName: String

  val boxIdsToOpen: IndexedSeq[BoxId]

  val fee: Long

  val timestamp: Long

  val attestation: Map[P, PR]

  val newBoxes: Traversable[BX]

  override def toString: String = this.transactionName + Transaction.jsonEncoder(this).noSpaces

}


object Transaction {
  type TransactionId = ModifierId
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (2: Byte)

  def nonceFromDigest (digest: Digest32): Box.Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  implicit def jsonEncoder[T, P <: Proposition, PR <: Proof[P]]: Encoder[Transaction[_, _, _, _]] = {
    case tx: CodeCreation           => CodeCreation.jsonEncoder(tx)
    case tx: ProgramCreation        => ProgramCreation.jsonEncoder(tx)
    case tx: ProgramMethodExecution => ProgramMethodExecution.jsonEncoder(tx)
    case tx: ProgramTransfer        => ProgramTransfer.jsonEncoder(tx)
    case tx: PolyTransfer           => PolyTransfer.jsonEncoder(tx)
    case tx: ArbitTransfer[P]   => ArbitTransfer.jsonEncoder[P](tx)
    case tx: AssetTransfer          => AssetTransfer.jsonEncoder(tx)
    case tx: AssetCreation          => AssetCreation.jsonEncoder(tx)
    case tx: Coinbase               => Coinbase.jsonEncoder(tx)
  }

  implicit val jsonDecoder: Decoder[Transaction[_, _, _, _]] = { c   : HCursor =>
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
