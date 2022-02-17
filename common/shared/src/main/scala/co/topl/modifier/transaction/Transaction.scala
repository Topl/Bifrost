package co.topl.modifier.transaction

import co.topl.attestation.{Proof, Proposition}
import co.topl.crypto.hash.digest.Digest32
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.BloomFilter.BloomTopic
import co.topl.modifier.box.{Box, BoxId}
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.{Identifiable, Identifier, Int128}
import com.google.common.primitives.Longs

import scala.collection.immutable.ListMap

abstract class Transaction[+T, P <: Proposition](implicit val identifiableEv: Identifiable[P])
    extends NodeViewModifier {

  override lazy val id: ModifierId = ModifierId.create(this).getOrThrow()

  val modifierTypeId: ModifierTypeId = Transaction.modifierTypeId

  val bloomTopics: IndexedSeq[BloomTopic]

  val boxIdsToOpen: IndexedSeq[BoxId]

  val newBoxes: Iterable[Box[T]]

  val attestation: ListMap[P, Proof[P]]

  val fee: Int128

  val timestamp: Long

  def messageToSign: Array[Byte] =
    Array(Transaction.identifier(this).typePrefix) ++
    newBoxes.foldLeft(Array[Byte]())((acc, x) => acc ++ x.bytes) ++
    boxIdsToOpen.foldLeft(Array[Byte]())((acc, x) => acc ++ x.hash.value) ++
    Longs.toByteArray(timestamp) ++
    fee.toByteArray

//  @deprecated
//  override def toString: String = ???

  def getPropIdentifier: Identifier = Identifiable[P].getId

}

object Transaction {
  type TX = Transaction[_, _ <: Proposition]
  type TxType = Byte
  type TransactionId = ModifierId

  val modifierTypeId: ModifierTypeId = ModifierTypeId(2: Byte)

  def updateAttestation[
    P <: Proposition
  ](tx: Transaction[_, P])(f: Array[Byte] => ListMap[P, Proof[P]]): ListMap[P, Proof[P]] =
    tx.attestation ++ f(tx.messageToSign)

  def nonceFromDigest(digest: Digest32): Box.Nonce =
    Longs.fromByteArray(digest.value.take(Longs.BYTES))

  def identifier(tx: TX): Identifier = tx match {
    case _: PolyTransfer[_]  => PolyTransfer.identifier.getId
    case _: ArbitTransfer[_] => ArbitTransfer.identifier.getId
    case _: AssetTransfer[_] => AssetTransfer.identifier.getId
  }
}
