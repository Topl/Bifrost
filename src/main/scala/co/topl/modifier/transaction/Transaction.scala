package co.topl.modifier.transaction

import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.attestation.EvidenceProducer
import co.topl.attestation.proof.Proof
import co.topl.attestation.proposition.{Proposition, PublicKeyPropositionCurve25519, ThresholdPropositionCurve25519}
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.transaction.Transaction.TxType
import co.topl.modifier.transaction.serialization.TransactionSerializer
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.state.box.{Box, BoxId}
import co.topl.utils.serialization.BifrostSerializer
import com.google.common.primitives.Longs
import io.circe.{Decoder, Encoder, HCursor}
import scorex.crypto.hash.{Blake2b256, Digest32}

import scala.util.Try

abstract class Transaction[T, P <: Proposition: EvidenceProducer, PR <: Proof[P], BX <: Box[T]] extends NodeViewModifier {

  override type M = Transaction[_,_,_,_]

  override lazy val id: ModifierId = ModifierId(Blake2b256(messageToSign))

  override lazy val serializer: BifrostSerializer[Transaction[_,_,_,_]] =
    TransactionSerializer

  lazy val bloomTopics: Option[IndexedSeq[Array[Byte]]] = None

  lazy val messageToSign: Array[Byte] =
    Array(txTypePrefix) ++
      newBoxes.foldLeft(Array[Byte]())((acc, x) => acc ++ x.bytes) ++
      boxIdsToOpen.foldLeft(Array[Byte]())((acc, x) => acc ++ x.hashBytes) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)

  val modifierTypeId: ModifierTypeId = Transaction.modifierTypeId

  val txTypePrefix: TxType

  val boxIdsToOpen: IndexedSeq[BoxId]

  val newBoxes: Traversable[BX]

  val attestation: Map[P, PR]

  val fee: Long

  val timestamp: Long

  override def toString: String = Transaction.prefixToTypeString(txTypePrefix) + Transaction.jsonEncoder(this).noSpaces

  def getPropositionType: P = attestation.head._1

  def semanticValidate (stateReader: StateReader)(implicit networkPrefix: NetworkPrefix): Try[Unit]

  def syntacticValidate (implicit networkPrefix: NetworkPrefix): Try[Unit]

}


object Transaction {
  type TransactionId = ModifierId
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (2: Byte)

  type TxType = Byte

  def nonceFromDigest (digest: Digest32): Box.Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  def prefixToTypeString(prefix: TxType): String = prefix match {
    case ArbitTransfer.txTypePrefix     => "ArbitTransfer"
    case PolyTransfer.txTypePrefix      => "PolyTransfer"
    case AssetTransfer.txTypePrefix     => "AssetTransfer"
  }

  implicit def jsonEncoder: Encoder[Transaction[_, _ <: Proposition, _ <: Proof[_], _ <: Box[_]]] = {
    //    case tx: CodeCreation           => CodeCreation.jsonEncoder(tx)
    //    case tx: ProgramCreation        => ProgramCreation.jsonEncoder(tx)
    //    case tx: ProgramMethodExecution => ProgramMethodExecution.jsonEncoder(tx)
    //    case tx: ProgramTransfer        => ProgramTransfer.jsonEncoder(tx)
    case tx: PolyTransfer[_,_]    => PolyTransfer.jsonEncoder(tx)
    case tx: ArbitTransfer[_,_]   => ArbitTransfer.jsonEncoder(tx)
    case tx: AssetTransfer[_,_]   => AssetTransfer.jsonEncoder(tx)
  }

  implicit val jsonDecoder: Decoder[Transaction[_, _ <: Proposition, _ <: Proof[_], _ <: Box[_]]] = { c: HCursor =>
    c.downField("txType").as[String].map {
//      case "CodeCreation"           => CodeCreation.jsonDecoder(c)
//      case "ProgramCreation"        => ProgramCreation.jsonDecoder(c)
//      case "ProgramMethodExecution" => ProgramMethodExecution.jsonDecoder(c)
//      case "ProgramTransfer"        => ProgramTransfer.jsonDecoder(c)
      case "PolyTransfer"           => PolyTransfer.jsonDecoder(c)
      case "ArbitTransfer"          => ArbitTransfer.jsonDecoder(c)
      case "AssetTransfer"          => AssetTransfer.jsonDecoder(c)
    } match {
      case Right(tx) => tx
      case Left(ex)  => throw ex
    }
  }
}
