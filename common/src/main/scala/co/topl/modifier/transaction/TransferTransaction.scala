package co.topl.modifier.transaction

import co.topl.attestation._
import co.topl.crypto.hash.blake2b256
import co.topl.modifier.block.BloomFilter.BloomTopic
import co.topl.modifier.box._
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.codecs.binary._
import co.topl.utils.{Identifiable, Int128}
import com.google.common.primitives.{Ints, Longs}
import io.circe.Json
import io.circe.syntax.EncoderOps

import scala.collection.immutable.ListMap

abstract class TransferTransaction[
  +T <: TokenValueHolder,
  P <: Proposition
](
  val from:                        IndexedSeq[(Address, Box.Nonce)],
  val to:                          IndexedSeq[(Address, T)],
  val attestation:                 ListMap[P, Proof[P]],
  val fee:                         Int128,
  val timestamp:                   Long,
  val data:                        Option[Latin1Data],
  val minting:                     Boolean
)(implicit val evidenceProducerEv: EvidenceProducer[P], identifiableEv: Identifiable[P])
    extends Transaction[TokenValueHolder, P] {

  lazy val bloomTopics: IndexedSeq[BloomTopic] = to.map(b => BloomTopic(b._1.bytes))

  lazy val boxIdsToOpen: IndexedSeq[BoxId] = from.map { case (addr, nonce) =>
    BoxId.idFromEviNonce(addr.evidence, nonce)
  }

  val (feeOutputParams, coinOutputParams) = TransferTransaction.calculateBoxNonce[T](this, to)

  val feeChangeOutput: PolyBox =
    PolyBox(feeOutputParams.evidence, feeOutputParams.nonce, feeOutputParams.value)

  val coinOutput: Iterable[TokenBox[T]]

  override def messageToSign: Array[Byte] =
    super.messageToSign ++ data.fold(Array(0: Byte))(_.value) :+ (if (minting) 1: Byte else 0: Byte)

  override val newBoxes: Iterable[TokenBox[TokenValueHolder]]

}

object TransferTransaction {

  case class BoxParams[+T <: TokenValueHolder](evidence: Evidence, nonce: Box.Nonce, value: T)

  def encodeFrom(from: IndexedSeq[(Address, Box.Nonce)]): Json =
    from.map(x => (x._1.asJson, x._2.toString.asJson)).asJson

  /**
   * Computes a unique nonce value based on the transaction type and
   * inputs and returns the details needed to create the output boxes for the transaction
   */
  def calculateBoxNonce[T <: TokenValueHolder](
    tx: TransferTransaction[T, _ <: Proposition],
    to: IndexedSeq[(Address, T)]
  ): (BoxParams[SimpleValue], Iterable[BoxParams[T]]) = {

    // known input data (similar to messageToSign but without newBoxes since they aren't known yet)
    val txIdPrefix = Transaction.identifier(tx).typePrefix
    val boxIdsToOpenAccumulator = tx.boxIdsToOpen.foldLeft(Array[Byte]())((acc, x) => acc ++ x.hash.value)
    val timestampBytes = Longs.toByteArray(tx.timestamp)
    val feeBytes = tx.fee.toByteArray

    val inputBytes =
      Array(txIdPrefix) ++ boxIdsToOpenAccumulator ++ timestampBytes ++ feeBytes

    val calcNonce: Int => Box.Nonce = (index: Int) => {
      val digest = blake2b256.hash(inputBytes ++ Ints.toByteArray(index))
      Transaction.nonceFromDigest(digest)
    }

    val feeChangeParams = BoxParams(tx.to.head._1.evidence, calcNonce(0), SimpleValue(tx.to.head._2.quantity))

    val coinOutputParams: IndexedSeq[BoxParams[T]] =
      to.tail.zipWithIndex
        .map { case ((addr, value), idx) =>
          BoxParams(addr.evidence, calcNonce(idx + 1), value)
        }
    (feeChangeParams, coinOutputParams)
  }

}
