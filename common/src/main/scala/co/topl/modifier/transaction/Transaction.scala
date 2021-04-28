package co.topl.modifier.transaction

import co.topl.attestation.{Address, Proof, Proposition}
import co.topl.crypto.BytesOf
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.BloomFilter.BloomTopic
import co.topl.modifier.box.{Box, BoxId, ProgramId}
import co.topl.modifier.{BoxReader, ModifierId, NodeViewModifier}
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.{Identifiable, Identifier, Int128}
import com.google.common.primitives.Longs
import io.circe.{Decoder, Encoder, HCursor}
import co.topl.crypto.hash.Digest32
import co.topl.crypto.Implicits._

import scala.util.Try

abstract class Transaction[+T, P <: Proposition: Identifiable] extends NodeViewModifier {

  override lazy val id: ModifierId = ModifierId(this)

  val modifierTypeId: ModifierTypeId = Transaction.modifierTypeId

  val bloomTopics: IndexedSeq[BloomTopic]

  val boxIdsToOpen: IndexedSeq[BoxId]

  val newBoxes: Traversable[Box[T]]

  val attestation: Map[P, Proof[P]]

  val fee: Int128

  val timestamp: Long

  override def toString: String =
    Transaction.identifier(this).typeString + Transaction.jsonEncoder(this).noSpaces

  def messageToSign: Array[Byte] =
    Array(Transaction.identifier(this).typePrefix) ++
    newBoxes.foldLeft(Array[Byte]())((acc, x) => acc ++ x.bytes) ++
    boxIdsToOpen.foldLeft(Array[Byte]())((acc, x) => acc ++ x.hashBytes) ++
    Longs.toByteArray(timestamp) ++
    fee.toByteArray

  def getPropIdentifier: Identifier = Identifiable[P].getId

  def semanticValidate(boxReader: BoxReader[ProgramId, Address])(implicit networkPrefix: NetworkPrefix): Try[Unit]

  def syntacticValidate(implicit networkPrefix: NetworkPrefix): Try[Unit]

  def rawValidate(implicit networkPrefix: NetworkPrefix): Try[Unit]

}

object Transaction {
  type TX = Transaction[_, _ <: Proposition]
  type TxType = Byte
  type TransactionId = ModifierId

  val modifierTypeId: ModifierTypeId = ModifierTypeId(2: Byte)

  def updateAttestation[
    P <: Proposition
  ](tx: Transaction[_, P])(f: Array[Byte] => Map[P, Proof[P]]): Map[P, Proof[P]] =
    tx.attestation ++ f(tx.messageToSign)

  def nonceFromDigest(digest: Digest32): Box.Nonce =
    Longs.fromByteArray(BytesOf[Digest32].take(digest, Longs.BYTES))

  def identifier(tx: TX): Identifier = tx match {
    case _: PolyTransfer[_]  => PolyTransfer.identifier.getId
    case _: ArbitTransfer[_] => ArbitTransfer.identifier.getId
    case _: AssetTransfer[_] => AssetTransfer.identifier.getId
  }

  implicit def jsonTypedEncoder[T, P <: Proposition]: Encoder[Transaction[T, P]] = { case tx: Transaction[_, _] =>
    jsonEncoder(tx)
  }

  implicit def jsonEncoder: Encoder[TX] = {
    //    case tx: CodeCreation           => CodeCreation.jsonEncoder(tx)
    //    case tx: ProgramCreation        => ProgramCreation.jsonEncoder(tx)
    //    case tx: ProgramMethodExecution => ProgramMethodExecution.jsonEncoder(tx)
    //    case tx: ProgramTransfer        => ProgramTransfer.jsonEncoder(tx)
    case tx: PolyTransfer[_]  => PolyTransfer.jsonEncoder(tx)
    case tx: ArbitTransfer[_] => ArbitTransfer.jsonEncoder(tx)
    case tx: AssetTransfer[_] => AssetTransfer.jsonEncoder(tx)
  }

  implicit def jsonDecoder(implicit networkPrefix: NetworkPrefix): Decoder[TX] = { c: HCursor =>
    c.downField("txType").as[String].map {
//      case "CodeCreation"           => CodeCreation.jsonDecoder(c)
//      case "ProgramCreation"        => ProgramCreation.jsonDecoder(c)
//      case "ProgramMethodExecution" => ProgramMethodExecution.jsonDecoder(c)
//      case "ProgramTransfer"        => ProgramTransfer.jsonDecoder(c)
      case PolyTransfer.typeString  => PolyTransfer.jsonDecoder(networkPrefix)(c)
      case ArbitTransfer.typeString => ArbitTransfer.jsonDecoder(networkPrefix)(c)
      case AssetTransfer.typeString => AssetTransfer.jsonDecoder(networkPrefix)(c)
    } match {
      case Right(tx) => tx
      case Left(ex)  => throw ex
    }
  }
}
