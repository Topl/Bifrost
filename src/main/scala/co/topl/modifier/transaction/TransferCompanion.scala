package co.topl.modifier.transaction

import co.topl.crypto.{PrivateKey25519, Signature25519}
import co.topl.modifier.transaction.Transaction.{Nonce, Value}
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.state.box._
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition

import scala.util.Try

trait TransferCompanion {

  type SR = StateReader[Box]

  /**
   *
   * @param from
   * @param to
   * @param fee
   * @param timestamp
   * @param txType
   * @param extraArgs
   * @return
   */
  def parametersForApply ( from: IndexedSeq[(PrivateKey25519, Nonce)],
                           to: IndexedSeq[(PublicKey25519Proposition, Value)],
                           fee      : Long,
                           timestamp: Long,
                           txType   : String,
                           extraArgs: Any*
                         ):
  Try[(IndexedSeq[(PublicKey25519Proposition, Nonce)], Map[PublicKey25519Proposition, Signature25519])] = Try {
    val fromPub = from.map { case (pr, n) => pr.publicImage -> n }

    val undersigned = txType match {
      case "PolyTransfer"  => PolyTransfer(fromPub, to, Map(), fee, timestamp, extraArgs(0).asInstanceOf[String])
      case "ArbitTransfer" => ArbitTransfer(fromPub, to, Map(), fee, timestamp, extraArgs(0).asInstanceOf[String])
      case "AssetTransfer" => AssetTransfer(fromPub, to, Map(), extraArgs(0).asInstanceOf[PublicKey25519Proposition],
                                            extraArgs(1).asInstanceOf[String], fee, timestamp, extraArgs(2).asInstanceOf[String])
    }

    val msg = undersigned.messageToSign
    val sigs = from.map { case (priv, _) => (priv.publicImage, priv.sign(msg)) }.toMap
    (fromPub, sigs)
  }

  /**
   *
   * @param state
   * @param toReceive
   * @param sender
   * @param fee
   * @param txType
   * @param extraArgs
   * @return
   */
  def parametersForCreate ( state    : SR,
                            toReceive: IndexedSeq[(PublicKey25519Proposition, Long)],
                            sender   : IndexedSeq[PublicKey25519Proposition],
                            fee      : Long,
                            txType   : String,
                            extraArgs: Any*
                          ):
  (IndexedSeq[(PublicKey25519Proposition, Long, Long)], IndexedSeq[(PublicKey25519Proposition, Long)]) = {

    toReceive
      .foldLeft((IndexedSeq[(PublicKey25519Proposition, Long, Long)](), IndexedSeq[(PublicKey25519Proposition, Long)]())) {
        case (a, (recipient, amount)) =>

          // Restrict box search to specified public keys if provided
          val keyFilteredBoxes = sender.flatMap(s => state.getTokenBoxes(s)).flatten

          // Match only the type of boxes specified by txType
          val keyAndTypeFilteredBoxes: Seq[TokenBox] = txType match {
            case "PolyTransfer"  =>
              keyFilteredBoxes.flatMap(_ match {
                                         case p: PolyBox => Some(p)
                                         case _          => None
                                       })
            case "ArbitTransfer" =>
              keyFilteredBoxes.flatMap(_ match {
                                         case a: ArbitBox => Some(a)
                                         case _           => None
                                       })
            case "AssetTransfer" =>
              if ( extraArgs(2).asInstanceOf[Option[String]].isDefined ) {
                keyFilteredBoxes.flatMap(_ match {
                                           case a: AssetBox
                                             if (a.id equals extraArgs(2).asInstanceOf[Option[String]].get) =>
                                             Some(a)
                                         })
              } else {
                keyFilteredBoxes.flatMap(_ match {
                                           case a: AssetBox
                                             if (a.assetCode equals extraArgs(1).asInstanceOf[String]) &&
                                               (a.issuer equals extraArgs(0)
                                                 .asInstanceOf[PublicKey25519Proposition]) =>
                                             Some(a)
                                           case _                                          => None
                                         })
              }
          }

          if ( keyAndTypeFilteredBoxes.length < 1 ) throw new Exception("No boxes found to fund transaction")

          val senderInputBoxes: IndexedSeq[(PublicKey25519Proposition, Nonce, Long)] = keyAndTypeFilteredBoxes
            .map(b => (b.proposition, b.nonce, b.value))
            .toIndexedSeq

          // amount available to send in tx
          val canSend = senderInputBoxes.map(_._3).sum

          if ( canSend < amount + fee ) throw new Exception("Not enough funds to create transaction")

          require(canSend >= (toReceive.map(_._2).sum + fee))

          // Updated sender balance for specified box type (this is the change calculation for sender)
          //TODO reconsider? - returns change to first key in list
          val senderUpdatedBalance: (PublicKey25519Proposition, Long) = (sender.head, canSend - amount - fee)

          // create the list of outputs (senderChangeOut & recipientOut)
          val to: IndexedSeq[(PublicKey25519Proposition, Long)] = IndexedSeq(senderUpdatedBalance, (recipient, amount))

          require(senderInputBoxes.map(_._3).sum - to.map(_._2).sum == fee)
          (a._1 ++ senderInputBoxes, a._2 ++ to)
      }
  }

  /**
   *
   * @param tx
   * @param withSigs
   * @return
   */
  def validateTransfer ( tx: TransferTransaction, withSigs: Boolean = true ): Try[Unit] = Try {
    require(tx.to.forall(_._2 > 0L)) // amount sent must be greater than 0
    require(tx.fee >= 0) // fee must be non-negative
    require(tx.timestamp >= 0) // timestamp must be valid

    // prototype transactions do not contain signatures at creation
    if ( withSigs ) {
      require(tx.signatures.forall {
        case (prop, sign) => sign.isValid(prop, tx.messageToSign)
      })
      require(tx.from.forall {
        case (prop, nonce) => tx.signatures.contains(prop)
      })
    }

    // ensure that the input and output lists of box ids are unique
    require(tx.newBoxes.forall(b ⇒ !tx.boxIdsToOpen.contains(b.id)))
  }
}

//  //noinspection ScalaStyle
//  def parametersForCreate (tbr      : TokenBoxRegistry,
//                           state    : SR,
//                           w        : Wallet,
//                           toReceive: IndexedSeq[(PublicKey25519Proposition, Long)],
//                           sender   : IndexedSeq[PublicKey25519Proposition],
//                           fee      : Long,
//                           txType   : String,
//                           extraArgs: Any*
//                          ):
//  (IndexedSeq[(PrivateKey25519, Long, Long)], IndexedSeq[(PublicKey25519Proposition, Long)]) = {
//
//    toReceive
//      .foldLeft((IndexedSeq[(PrivateKey25519, Long, Long)](), IndexedSeq[(PublicKey25519Proposition, Long)]())) {
//        case (a, (recipient, amount)) =>
//
//          // Restrict box search to specified public keys if provided
//          val keyFilteredBoxes = sender.flatMap(s => state.getTokenBoxes(s)).flatten
//
//          // Match only the type of boxes specified by txType
//          val keyAndTypeFilteredBoxes: Seq[TokenBox] = txType match {
//            case "PolyTransfer"  =>
//              keyFilteredBoxes.flatMap(_ match {
//                case p: PolyBox => Some(p)
//                case _          => None
//              })
//            case "ArbitTransfer" =>
//              keyFilteredBoxes.flatMap(_ match {
//                case a: ArbitBox => Some(a)
//                case _           => None
//              })
//            case "AssetTransfer" =>
//              if (extraArgs(2).asInstanceOf[Option[String]].isDefined) {
//                keyFilteredBoxes.flatMap(_ match {
//                  case a: AssetBox
//                    if ( Base58.encode(a.id) equals extraArgs(2).asInstanceOf[Option[String]].get ) =>
//                    Some(a)
//                })
//              } else {
//                keyFilteredBoxes.flatMap(_ match {
//                  case a: AssetBox
//                    if ( a.assetCode equals extraArgs(1).asInstanceOf[String] ) &&
//                      ( a.issuer equals extraArgs(0)
//                        .asInstanceOf[PublicKey25519Proposition] ) =>
//                    Some(a)
//                  case _                                           => None
//                })
//              }
//          }
//
//          if (keyAndTypeFilteredBoxes.length < 1) throw new Exception("No boxes found to fund transaction")
//
//          //YT Note - Dust collection takes place here - so long as someone forms a valid transaction,
//          //YT Note - all their tokens of that type are collected into one spend box and one change box
//
//          // Check if the keys currently unlocked in wallet match the proposition of any of the found boxes
//          val senderInputBoxes: IndexedSeq[(PrivateKey25519, Long, Long)] = keyAndTypeFilteredBoxes
//            .flatMap {
//              b =>
//                w.secretByPublicImage(b.proposition)
//                  .map((_, b.nonce, b.value))
//            }
//            .toIndexedSeq
//
//          // amount available to send in tx
//          val canSend = senderInputBoxes.map(_._3).sum
//
//          if (canSend < amount + fee) throw new Exception("Not enough funds to create transaction")
//
//          // Updated sender balance for specified box type (this is the change calculation for sender)
//          val senderUpdatedBalance: (PublicKey25519Proposition, Long) = (sender.head, canSend - amount - fee)
//
//          // create the list of outputs (senderChangeOut & recipientOut)
//          val to: IndexedSeq[(PublicKey25519Proposition, Long)] = IndexedSeq(senderUpdatedBalance, (recipient, amount))
//
//          require(senderInputBoxes.map(_._3).sum - to.map(_._2).sum == fee)
//          (a._1 ++ senderInputBoxes, a._2 ++ to)
//      }
//  }
