package bifrost.transaction.bifrostTransaction

import bifrost.scorexMod.GenericWalletBox
import BifrostTransaction.{Nonce, Value}
import bifrost.bfr.BFR
import bifrost.transaction.box._
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import bifrost.wallet.BWallet
import com.google.common.primitives.Longs
import scorex.crypto.encode.Base58

import scala.util.Try

trait NewTransferUtil {

//  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))
//
//  def parametersForApply(from: IndexedSeq[(PrivateKey25519, Nonce)],
//                         to: IndexedSeq[(PublicKey25519Proposition, Value)],
//                         fee: Long,
//                         timestamp: Long,
//                         txType: String,
//                         extraArgs: Any*):
//  Try[(IndexedSeq[(PublicKey25519Proposition, Nonce)], IndexedSeq[Signature25519])] = Try {
//    val fromPub = from.map { case (pr, n) => pr.publicImage -> n }
//    val fakeSigs = from.map(_ => Signature25519(Array()))
//
//    val undersigned = txType match {
//      case "PolyTransfer" => PolyTransfer(fromPub, to, fakeSigs, fee, timestamp, extraArgs(0).asInstanceOf[String])
//      case "ArbitTransfer" => ArbitTransfer(fromPub, to, fakeSigs, fee, timestamp, extraArgs(0).asInstanceOf[String])
//      case "AssetTransfer" => AssetTransfer(
//        fromPub,
//        to,
//        fakeSigs,
//        extraArgs(0).asInstanceOf[PublicKey25519Proposition],
//        extraArgs(1).asInstanceOf[String],
//        fee,
//        timestamp,
//        extraArgs(2).asInstanceOf[String]
//      )
//    }
//
//    val msg = undersigned.messageToSign
//    val sigs = from.map { case (priv, _) => PrivateKey25519Companion.sign(priv, msg) }
//    (fromPub, sigs)
//  }


//  def parametersForCreate(bfr: BFR,
//                          toReceive: IndexedSeq[(PublicKey25519Proposition, Long)],
//                          sender: PublicKey25519Proposition,
//                          fee: Long,
//                          txType: String,
//                          extraArgs: Any*):
//  (IndexedSeq[(PublicKey25519Proposition, Long, Long)], IndexedSeq[(PublicKey25519Proposition, Long)]) = {
//
//    toReceive
//      .foldLeft((IndexedSeq[(PublicKey25519Proposition, Long, Long)](), IndexedSeq[(PublicKey25519Proposition, Long)]())) {
//        case (a, (recipient, amount)) =>
//
//          // Restrict box search to specified public keys if provided
//          val keyFilteredBoxes: Seq[BifrostBox] =
//            bfr.boxesByKey(sender)
//
//          // Match only the type of boxes specified by txType
//          val keyAndTypeFilteredBoxes: Seq[BifrostPublic25519NoncedBox] = txType match {
//            case "PolyTransfer" =>
//              keyFilteredBoxes.flatMap(_ match {
//                case p: PolyBox => Some(p)
//                case _ => None
//              })
//            case "ArbitTransfer" =>
//              keyFilteredBoxes.flatMap(_ match {
//                case a: ArbitBox => Some(a)
//                case _ => None
//              })
//            case "AssetTransfer" =>
//              keyFilteredBoxes.flatMap(_ match {
//                case a: AssetBox
//                  if (a.assetCode equals extraArgs(1).asInstanceOf[String]) &&
//                    (a.issuer equals extraArgs(0)
//                      .asInstanceOf[PublicKey25519Proposition]) =>
//                  Some(a)
//                case _ => None
//              })
//          }
//
//          val senderInputBoxes: IndexedSeq[(PublicKey25519Proposition, Nonce, Long)] = keyAndTypeFilteredBoxes
//            .map (b => (b.proposition, b.nonce, b.value))
//            .toIndexedSeq
//
//          // amount available to send in tx
//          val canSend = senderInputBoxes.map(_._3).sum
//
//          require(canSend >= (toReceive.map(_._2).sum + fee))
//
//          // Updated sender balance for specified box type (this is the change calculation for sender)
//          val senderUpdatedBalance: (PublicKey25519Proposition, Long) = (sender, canSend - amount - fee)
//
//          // create the list of outputs (senderChangeOut & recipientOut)
//          val to: IndexedSeq[(PublicKey25519Proposition, Long)] = IndexedSeq(senderUpdatedBalance, (recipient, amount))
//
//          require(senderInputBoxes.map(_._3).sum - to.map(_._2).sum == fee)
//          (a._1 ++ senderInputBoxes, a._2 ++ to)
//      }
//  }

//  def validateTx(tx: TransferTransaction): Try[Unit] = Try {
//    require(tx.from.size == tx.signatures.size)
//    require(tx.to.forall(_._2 >= 0L))
//    require(tx.fee >= 0)
//    require(tx.timestamp >= 0)
//    require(tx.from.zip(tx.signatures).forall { case ((prop, _), proof) =>
//      proof.isValid(prop, tx.messageToSign)
//    })
//
//  }
}
