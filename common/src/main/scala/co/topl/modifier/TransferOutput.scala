package co.topl.modifier

import co.topl.attestation.{Address, Evidence, Proposition}
import co.topl.modifier.TransferOutput.ToInfo
import co.topl.modifier.box._
import co.topl.modifier.transaction._
import com.google.common.primitives.{Ints, Longs}
import scorex.crypto.hash.Blake2b256

case class TransferOutput[+T <: TokenValueHolder](feeChangeInfo: ToInfo[SimpleValue], coinInfo: IndexedSeq[ToInfo[T]]) {
  val all: IndexedSeq[ToInfo[TokenValueHolder]] = feeChangeInfo +: coinInfo

}

object TransferOutput {

  type ToInfo[+T] = (Address, T)

  /** Computes a unique nonce value based on the transaction type and
    * inputs and returns the details needed to create the output boxes for the transaction
    */
  def generateOutputBoxes[BX <: TokenBox[_]](
                                                  tx: TransferTransaction[_, _ <: Proposition]
                                                ): (PolyBox, IndexedSeq[BX]) = {
    ???
//    // known input data (similar to messageToSign but without newBoxes since they aren't known yet)
//    val txIdPrefix = Transaction.identifier(tx).typePrefix
//    val boxIdsToOpenAccumulator = tx.boxIdsToOpen.foldLeft(Array[Byte]())((acc, x) => acc ++ x.hashBytes)
//    val timestampBytes = Longs.toByteArray(tx.timestamp)
//    val feeBytes = tx.fee.toByteArray
//
//    val inputBytes =
//      Array(txIdPrefix) ++ boxIdsToOpenAccumulator ++ timestampBytes ++ feeBytes
//
//    val calcNonce: Int => Box.Nonce = (index: Int) => {
//      val digest = Blake2b256(inputBytes ++ Ints.toByteArray(index))
//      Transaction.nonceFromDigest(digest)
//    }
//
//    def coinBoxesGen(
//                                         toSeq: IndexedSeq[ToInfo[_ <: TokenValueHolder]]
//                                       )(f: (Evidence, Box.Nonce, TokenValueHolder) => Option[BX]): IndexedSeq[BX] =
//      toSeq.zipWithIndex
//        .map { case ((addr, value), idx) =>
//          f(addr.evidence, calcNonce(idx + 1), value) match {
//            case Some(value) => value
//            case None => throw new Exception("Invalid TokenValueHolder specified for coinOutput type")
//          }
//        }
//
//    // generate poly fee output
//    val feeChangeEvidence = tx.to.feeChangeInfo._1.evidence
//    val feeChangeNonce = calcNonce(0)
//    val feeChangeValue = SimpleValue(tx.to.feeChangeInfo._2.quantity)
//    val feeChangeOutput = PolyBox(feeChangeEvidence, feeChangeNonce, feeChangeValue)
//
//    // generate coin output
//    val coinOutputParams: IndexedSeq[BX] = {
//      tx match {
//        case ArbitTransfer(_, to, _, _, _, _, _) =>
//          coinBoxesGen(to.coinInfo)(ArbitBox.apply)
//        //        case AssetTransfer(from, to, attestation, fee, timestamp, data, minting) =>
//        //        case PolyTransfer(from, to, attestation, fee, timestamp, data, minting) =>
//      }
//    }
//
//    (feeChangeOutput, coinOutputParams)
  }
}
