package co.topl.modifier.transaction

import co.topl.attestation.EvidenceProducer.syntax._
import co.topl.attestation.{ Address, BoxUnlocker, EvidenceProducer }
import co.topl.attestation.proof.Proof
import co.topl.attestation.proposition.Proposition
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.state.box.{ ArbitBox, AssetBox, Box, BoxId, PolyBox, TokenBox }
import com.google.common.primitives.{ Ints, Longs }
import scorex.crypto.hash.{ Blake2b256, Digest32 }

import scala.util.{ Failure, Success, Try }

abstract class TransferTransaction[P <: Proposition, PR <: Proof[P]] ( val from: IndexedSeq[(Address, Box.Nonce)],
                                                                       val to: IndexedSeq[(Address, TokenBox.Value)],
                                                                       val attestation: Map[P, PR],
                                                                       val fee: Long,
                                                                       val timestamp: Long,
                                                                       val data: String
                                                                     ) extends Transaction[TokenBox.Value, P, PR, TokenBox] {

  lazy val boxIdsToOpen: IndexedSeq[BoxId] = from.map { case (addr, nonce) =>
    BoxId.idFromEviNonce(addr.evidence, nonce)
  }

  // todo: JAA - why not include the from bytes here? I assume this was intentional because the name is "no nonces" so was that a problem?
  lazy val hashNoNonces: Digest32 = Blake2b256(
    to.flatMap(_._1.bytes).toArray ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee) ++
      data.getBytes
  )

  override lazy val messageToSign: Array[Byte] =
    super.messageToSign ++
      to.flatMap(_._1.bytes).toArray ++
      data.getBytes
}




object TransferTransaction {

  def transactionDigest(tx: TransferTransaction[_ <: Proposition, _ <: Proof[_]]): Digest32 = {
    

    tx.to.filter(_._2 > 0L)
      .zipWithIndex
      .map {
        case ((addr, value), idx) =>
        Transaction.nonceFromDigest(
            Blake2b256(
              tx.transactionName.getBytes
                ++ addr.bytes
                ++ hashNoNonces
                ++ Ints.toByteArray(idx)))
      }
  }


  /**
   * Determines the input boxes needed to create a transfer transaction
   *
   * @param state a read-only version of the nodes current state
   * @param toReceive the recipients of boxes
   * @param sender the set of addresses that will contribute boxes to this transaction
   * @param fee the fee to be paid for the transaction
   * @param txType the type of transfer
   * @param assetArgs a tuple of asset specific details for finding the right asset boxes to be sent in a transfer
   * @return the input box information and output data needed to create the transaction case class
   */
  def createRawTransferTx ( state        : StateReader[TokenBox],
                            toReceive    : IndexedSeq[(Address, TokenBox.Value)],
                            sender       : IndexedSeq[Address],
                            changeAddress: Address,
                            fee          : TokenBox.Value,
                            txType       : String,
                            assetArgs    : Option[(Address, String, String)] = None // (issuer, assetCode, assetId)
                          ): Try[(IndexedSeq[(Address, Box.Nonce)], IndexedSeq[(Address, TokenBox.Value)])] = Try {

    // Lookup boxes for the given senders
    val senderBoxes =
      sender.flatMap { s =>
        state.getTokenBoxes(s)
          .getOrElse(throw new Exception("No boxes found to fund transaction")) // isn't this just an empty sequence instead of None?
          .map {
            case bx: PolyBox  => ("Poly", s, bx) // always get polys because this is how fees are paid
            case bx: ArbitBox if txType == "ArbitTransfer" => ("Arbit", s, bx)
            case bx: AssetBox if (txType == "AssetTransfer" &&
              bx.assetCode == assetArgs.getOrElse(throw new Error("Undefined assetCode parameter"))._2 &&
              bx.issuer == assetArgs.getOrElse(throw new Error("Undefined asset issuer parameter"))._1) => ("Asset", s, bx)
          }
      }.groupBy(_._1)

    // ensure there are enough polys to pay the fee
    require(senderBoxes("Poly").map(_._3.value).sum >= fee, s"Insufficient funds available to pay transaction fee.")

    // create the list of inputs and outputs (senderChangeOut & recipientOut)
    val (availableToSpend, inputs, outputs) = txType match {
      case "PolyTransfer"  =>
        (
          senderBoxes("Poly").map(_._3.value).sum - fee,
          senderBoxes("Poly").map(bxs => (bxs._2, bxs._3.nonce)),
          (changeAddress, senderBoxes("Poly").map(_._3.value).sum - fee - toReceive.map(_._2).sum) +: toReceive
        )

      case "ArbitTransfer" => senderBoxes("Arbit").map(_._3.value).sum
        (
          senderBoxes("Arbit").map(_._3.value).sum,
          senderBoxes("Arbit").map(bxs => (bxs._2, bxs._3.nonce)),
          (changeAddress, senderBoxes("Poly").map(_._3.value).sum - fee) +: toReceive
        )

      case "AssetTransfer" =>
        (
          senderBoxes("Asset").map(_._3.value).sum,
          senderBoxes("Asset").map(bxs => (bxs._2, bxs._3.nonce)),
          (changeAddress, senderBoxes("Poly").map(_._3.value).sum - fee) +: toReceive
        )
    }

    // ensure there are sufficient funds from the sender boxes to create all outputs
    require(availableToSpend >= (toReceive.map(_._2).sum), "Insufficient funds available to create transaction.")

    (inputs, outputs)
  }

  /**
   *
   * @param tx
   * @return
   */
  def validatePrototype[P <: Proposition: EvidenceProducer, PR <: Proof[P]] (tx: TransferTransaction[P, PR]): Try[Unit] =
    syntacticValidate(tx, withSigs = false)

  /**
   * Syntactic validation of a transfer transaction
   *
   * @param tx an instance of a transaction to check
   * @param withSigs boolean flag controlling whether signature verification should be checked or skipped
   * @return success or failure indicating the validity of the transaction
   */
  def syntacticValidate[
    P <: Proposition: EvidenceProducer,
    PR <: Proof[P]
  ] ( tx: TransferTransaction[P, PR],
      withSigs: Boolean = true): Try[Unit] = Try {

    require(tx.to.forall(_._2 > 0L), "Amount sent must be greater than 0")
    require(tx.fee > 0L, "Fee must be a non-zero positive value")
    require(tx.timestamp >= 0L, "Invalid timestamp")

    // prototype transactions do not contain signatures at creation
    if (withSigs) {
      // ensure that the signatures are valid signatures with the body of the transaction
      require(tx.attestation.forall {
        case (prop, proof) => proof.isValid(prop, tx.messageToSign)
      }, "The provided proposition is not satisfied by the given proof")

      // ensure that the propositions match the addresses given
      require(tx.from.forall {
        case (addr, _) => tx.attestation.keys.map(_.generateEvidence).toSeq.contains(addr.evidence)
      }, "The proposition(s) given do not match the evidence contained in the input boxes")
    }

    // ensure that the input and output lists of box ids are unique
    require(tx.newBoxes.forall(b ⇒ !tx.boxIdsToOpen.contains(b.id)), "The set of input box ids contains one or more of the output ids")
  }

  /**
   *
   * @param tx
   * @param state
   * @return
   */
  def semanticValidate[
    P <: Proposition: EvidenceProducer,
    PR <: Proof[P]
  ] ( tx: TransferTransaction[P, PR],
      state: StateReader[TokenBox] ): Try[Unit] = {

    // check that the transaction is correctly formed before checking state
    syntacticValidate(tx) match {
      case Failure(e) => throw e
      case _          => // continue processing
    }

    // compute transaction values used for validation
    val txOutput = tx.newBoxes.map(b => b.value).sum
    val unlockers = BoxUnlocker.generate(tx.from, tx.attestation)

    // iterate through the unlockers and sum up the value of the box for each valid unlocker
    unlockers.foldLeft[Try[Long]](Success(0L))(( trySum, unlocker ) => {
      trySum.flatMap { partialSum =>
        state.getBox(unlocker.closedBoxId) match {
          case Some(box: TokenBox) if unlocker.boxKey.isValid(unlocker.proposition, tx.messageToSign) =>
            Success(partialSum + box.value)

          case Some(_) => Failure(new Exception("Invalid unlocker"))
          case None    => Failure(new Exception(s"Box for unlocker $unlocker cannot be found in state"))
          case _       => Failure(new Exception("Invalid Box type for this transaction"))
        }
      }
    }) match {
      case Success(sum: Long) if txOutput == sum - tx.fee =>
        Success(Unit)

      case Success(sum: Long) =>
        Failure(new Exception(s"Tx output value not equal to input value. $txOutput != ${sum - tx.fee}"))

      case Failure(e) => throw e
    }
  }
}


