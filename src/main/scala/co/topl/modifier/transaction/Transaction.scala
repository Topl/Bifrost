package co.topl.modifier.transaction


import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.attestation.{Proof, Proposition}
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.BloomFilter.BloomTopic
import co.topl.modifier.transaction.Transaction.TxType
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.state.box.{Box, BoxId}
import co.topl.utils.HasName
import com.google.common.primitives.Longs
import io.circe.{Decoder, Encoder, HCursor}
import scorex.crypto.hash.Digest32

import scala.util.Try

abstract class Transaction[+T, P <: Proposition: HasName] extends NodeViewModifier {

  override lazy val id: ModifierId = ModifierId(this)

  val modifierTypeId: ModifierTypeId = Transaction.modifierTypeId

  val bloomTopics: IndexedSeq[BloomTopic]

  val txTypePrefix: TxType

  val boxIdsToOpen: IndexedSeq[BoxId]

  val newBoxes: Traversable[Box[T]]

  val attestation: Map[P, Proof[P]]

  val fee: Long

  val timestamp: Long

  override def toString: String =
    Transaction.getTypeString(this) + Transaction.jsonEncoder(this).noSpaces

  def messageToSign: Array[Byte] =
    Array(txTypePrefix) ++
      newBoxes.foldLeft(Array[Byte]())((acc, x) => acc ++ x.bytes) ++
      boxIdsToOpen.foldLeft(Array[Byte]())((acc, x) => acc ++ x.hashBytes) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)

  def getPropTypeString: String = HasName[P].name

  def semanticValidate (stateReader: StateReader)(implicit networkPrefix: NetworkPrefix): Try[Unit]

  def syntacticValidate (implicit networkPrefix: NetworkPrefix): Try[Unit]

  def rawValidate (implicit networkPrefix: NetworkPrefix): Try[Unit]

}


object Transaction {
  type TX = Transaction[_, _ <: Proposition]
  type TxType = Byte
  type TransactionId = ModifierId

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (2: Byte)

  def nonceFromDigest (digest: Digest32): Box.Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  def getTypeString[T <: TX](transaction: T): String = transaction match {
    case _: ArbitTransfer[_]     => ArbitTransfer.txTypeString
    case _: PolyTransfer[_]      => PolyTransfer.txTypeString
    case _: AssetTransfer[_]     => AssetTransfer.txTypeString
  }

  implicit def jsonTypedEncoder[T, P <: Proposition]: Encoder[Transaction[T, P]] = {
    case tx: Transaction[_,_] => jsonEncoder(tx)
  }

  implicit def jsonEncoder: Encoder[TX] = {
    //    case tx: CodeCreation           => CodeCreation.jsonEncoder(tx)
    //    case tx: ProgramCreation        => ProgramCreation.jsonEncoder(tx)
    //    case tx: ProgramMethodExecution => ProgramMethodExecution.jsonEncoder(tx)
    //    case tx: ProgramTransfer        => ProgramTransfer.jsonEncoder(tx)
    case tx: PolyTransfer[_]    => PolyTransfer.jsonEncoder(tx)
    case tx: ArbitTransfer[_]   => ArbitTransfer.jsonEncoder(tx)
    case tx: AssetTransfer[_]   => AssetTransfer.jsonEncoder(tx)
  }

  implicit def jsonDecoder: Decoder[TX] = { c: HCursor =>
    c.downField("txType").as[String].map {
//      case "CodeCreation"           => CodeCreation.jsonDecoder(c)
//      case "ProgramCreation"        => ProgramCreation.jsonDecoder(c)
//      case "ProgramMethodExecution" => ProgramMethodExecution.jsonDecoder(c)
//      case "ProgramTransfer"        => ProgramTransfer.jsonDecoder(c)
      case PolyTransfer.txTypeString           => PolyTransfer.jsonDecoder(c)
      case ArbitTransfer.txTypeString          => ArbitTransfer.jsonDecoder(c)
      case AssetTransfer.txTypeString          => AssetTransfer.jsonDecoder(c)
    } match {
      case Right(tx) => tx
      case Left(ex)  => throw ex
    }
  }
}
