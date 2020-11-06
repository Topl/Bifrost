package co.topl.modifier.transaction

import co.topl.attestation.EvidenceProducer.syntax._
import co.topl.attestation.proof.Proof
import co.topl.attestation.proposition.Proposition
import co.topl.attestation.{Address, EvidenceProducer}
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.state.box._

import scala.util.Try

trait TransferCompanion {

  type SR = StateReader[Box[_]]

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
  def parametersForCreate (state    : SR,
                           toReceive: IndexedSeq[(Address, TokenBox.Value)],
                           sender   : IndexedSeq[Address],
                           fee      : Long,
                           txType   : String,
                           extraArgs: Any*
                          ): (IndexedSeq[(Address, Long, Long)], IndexedSeq[(Address, Long)]) = {

    toReceive
      .foldLeft((IndexedSeq[(Address, Box.Nonce, TokenBox.Value)](), IndexedSeq[(Address, TokenBox.Value)]())) {
        case (acc, (recipient, amount)) =>

          // Lookup boxes for the given addresses
          val senderInputBoxes = sender.flatMap { s =>
            state.getTokenBoxes(s)
              .getOrElse(throw new Exception("No boxes found to fund transaction"))
              .map {
                case bx: PolyBox => (s, bx.nonce, bx.value)
                case bx: ArbitBox if txType == "ArbitTransfer" => (s, bx.nonce, bx.value)
                case bx: AssetBox if txType == "AssetTransfer" => (s, bx.nonce, bx.value)
              }
          }



          // amount available to send in tx
          val canSend = senderInputBoxes.map(_._3).sum

          require(canSend >= (toReceive.map(_._2).sum + fee), "Not enough funds to create transaction")

          // Updated sender balance for specified box type (this is the change calculation for sender)
          //TODO JAA - reconsider how change is sent - currently returns change to first sender in list
          val senderUpdatedBalance: (Address, TokenBox.Value) = (sender.head, canSend - amount - fee)

          // create the list of outputs (senderChangeOut & recipientOut)
          val to: IndexedSeq[(Address, TokenBox.Value)] = IndexedSeq(senderUpdatedBalance, (recipient, amount))

          require(senderInputBoxes.map(_._3).sum - to.map(_._2).sum == fee)
          (acc._1 ++ senderInputBoxes, acc._2 ++ to)

//          // Match only the type of boxes specified by txType
//          val keyAndTypeFilteredBoxes: Seq[TokenBox] = txType match {
//            case "PolyTransfer"  =>
//              senderBoxes.flatMap(_ match {
//                case p: PolyBox => Some(p)
//                case _          => None
//              })
//            case "ArbitTransfer" =>
//              senderBoxes.flatMap(_ match {
//                case a: ArbitBox => Some(a)
//                case _           => None
//              })
//            case "AssetTransfer" =>
//              if ( extraArgs(2).asInstanceOf[Option[String]].isDefined ) {
//                senderBoxes.flatMap(_ match {
//                  case a: AssetBox
//                    if (a.id equals extraArgs(2).asInstanceOf[Option[String]].get) =>
//                    Some(a)
//                })
//              } else {
//                senderBoxes.flatMap(_ match {
//                  case a: AssetBox
//                    if (a.assetCode equals extraArgs(1).asInstanceOf[String]) &&
//                      (a.issuer equals extraArgs(0)
//                        .asInstanceOf[PublicKeyCurve25519Proposition]) =>
//                    Some(a)
//                  case _                                               => None
//                })
//              }
//          }
      }
  }

  /**
   * Syntactic validation of a transfer transaction
    *
   * @param tx an instance of a transaction to check
   * @param withSigs boolean flag controlling whether signature verification should be checked or skipped
   * @return success or failure indicating the validity of the transaction
   */
  def syntacticValidateTransfer[
    P <: Proposition: EvidenceProducer,
    PR <: Proof[P]
  ] (tx: TransferTransaction[P, PR],
     withSigs: Boolean = true): Try[Unit] = Try {
    require(tx.to.forall(_._2 > 0L)) // amount sent must be greater than 0
    require(tx.fee >= 0) // fee must be non-negative
    require(tx.timestamp >= 0) // timestamp must be valid

    // prototype transactions do not contain signatures at creation
    if ( withSigs ) {
      // ensure that the signatures are valid signatures with the body of the transaction
      require(tx.attestation.forall {
        case (prop, proof) => proof.isValid(prop, tx.messageToSign)
      })

      // ensure that the propositions match the addresses given
      require(tx.from.forall {
        case (addr, _) => tx.attestation.keys.map(_.generateEvidence).toSeq.contains(addr.evidence)
      })
    }

    // ensure that the input and output lists of box ids are unique
    require(tx.newBoxes.forall(b ⇒ !tx.boxIdsToOpen.contains(b.id)))
  }
}
