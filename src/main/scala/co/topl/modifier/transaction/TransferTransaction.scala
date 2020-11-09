package co.topl.modifier.transaction

import co.topl.attestation.EvidenceProducer.syntax._
import co.topl.attestation.proof.Proof
import co.topl.attestation.proposition.Proposition
import co.topl.attestation.{ Address, BoxUnlocker, Evidence, EvidenceProducer }
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.state.box._
import com.google.common.primitives.Ints
import scorex.util.encode.Base58
import scorex.crypto.hash.Blake2b256

import scala.util.{ Failure, Success, Try }

abstract class TransferTransaction[P <: Proposition, PR <: Proof[P]] ( val from: IndexedSeq[(Address, Box.Nonce)],
                                                                       val to: IndexedSeq[(Address, TokenBox.Value)],
                                                                       val attestation: Map[P, PR],
                                                                       val fee: Long,
                                                                       val timestamp: Long,
                                                                       val data: String,
                                                                       val minting: Boolean
                                                                     ) extends Transaction[TokenBox.Value, P, PR, TokenBox] {

  lazy val boxIdsToOpen: IndexedSeq[BoxId] = from.map { case (addr, nonce) =>
    BoxId.idFromEviNonce(addr.evidence, nonce)
  }

  override lazy val messageToSign: Array[Byte] =
    super.messageToSign ++
      data.getBytes :+ (if (minting) 1: Byte else 0: Byte)
}


object TransferTransaction {

  /** Computes a unique nonce value based on the transaction inputs and returns the details needed to create the output boxes for the transaction */
  def boxParams(tx: TransferTransaction[_ <: Proposition, _ <: Proof[_]]): Traversable[(Evidence, Box.Nonce, TokenBox.Value)] = {
    val inputBytes = tx.boxIdsToOpen.foldLeft(Array[Byte]())((acc, x) => acc ++ x.hashBytes)

    tx.to
      .filter(_._2 > 0L)
      .zipWithIndex
      .map { case ((addr, value), idx) =>
        val digest = Blake2b256(inputBytes ++ Ints.toByteArray(idx))
        val nonce = Transaction.nonceFromDigest(digest)

        (addr.evidence, nonce, value)
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
  def createRawTransferParams ( state: StateReader[TokenBox],
                                toReceive: IndexedSeq[(Address, TokenBox.Value)],
                                sender: IndexedSeq[Address],
                                changeAddress: Address,
                                fee: TokenBox.Value,
                                txType: String,
                                assetArgs: Option[(Address, String)] = None // (issuer, assetCode)
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
    require(tx.from.nonEmpty, "Transaction must specify at least one input box")
    require(tx.fee >= 0L, "Fee must be a positive value")
    require(tx.timestamp >= 0L, "Invalid timestamp")
    require(Base58.decode(tx.data).fold(_ => false, _.length <= 128), "Data field must be less than 128 bytes") // todo: JAA - check that this works with empty data


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
   * Checks the stateful validity of a transaction
   *
   * @param tx the transaction to check
   * @param state the state to check the validity against
   * @return a success or failure denoting the result of this check
   */
  def semanticValidate[
    P <: Proposition: EvidenceProducer,
    PR <: Proof[P]
  ] ( tx: TransferTransaction[P, PR],
      state: StateReader[Box[_]] ): Try[Unit] = {

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
      // a normal transfer will fall in this case
      case Success(sum: Long) if txOutput == sum - tx.fee =>
        Success(Unit)

      // a minting transaction (of either Arbit, Polys, or Assets) will fall in this case
      case Success(_: Long) if tx.minting =>
        Success(Unit)

      case Success(sum: Long) if !tx.minting && txOutput != sum - tx.fee =>
        Failure(new Exception(s"Tx output value does not equal input value for non-minting transaction. $txOutput != ${sum - tx.fee}"))

      case Failure(e) => Failure(e)
    }
  }
}


