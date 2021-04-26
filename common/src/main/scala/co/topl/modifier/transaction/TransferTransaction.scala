package co.topl.modifier.transaction

import co.topl.attestation.EvidenceProducer.Syntax._
import co.topl.attestation.{Evidence, _}
import co.topl.modifier.BoxReader
import co.topl.modifier.block.BloomFilter.BloomTopic
import co.topl.modifier.box.{Box, _}
import co.topl.utils.Extensions.StringOps
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.{Identifiable, Int128}
import com.google.common.primitives.{Ints, Longs}
import io.circe.Json
import io.circe.syntax.EncoderOps
import co.topl.crypto.hash.{Blake2b256, Digest32, Hash}

import scala.util.{Failure, Success, Try}

abstract class TransferTransaction[
  +T <: TokenValueHolder,
  P <: Proposition: EvidenceProducer: Identifiable
](
  val from:        IndexedSeq[(Address, Box.Nonce)],
  val to:          IndexedSeq[(Address, T)],
  val attestation: Map[P, Proof[P]],
  val fee:         Int128,
  val timestamp:   Long,
  val data:        Option[String],
  val minting:     Boolean
) extends Transaction[T, P] {

  lazy val bloomTopics: IndexedSeq[BloomTopic] = to.map(b => BloomTopic(b._1.bytes))

  lazy val boxIdsToOpen: IndexedSeq[BoxId] = from.map { case (addr, nonce) =>
    BoxId.idFromEviNonce(addr.evidence, nonce)
  }

  override def messageToSign: Array[Byte] =
    super.messageToSign ++
    data.fold(Array(0: Byte))(_.getBytes) :+ (if (minting) 1: Byte else 0: Byte)

  def semanticValidate(boxReader: BoxReader[ProgramId, Address])(implicit networkPrefix: NetworkPrefix): Try[Unit] =
    TransferTransaction.semanticValidate(this, boxReader)

  def syntacticValidate(implicit networkPrefix: NetworkPrefix): Try[Unit] =
    TransferTransaction.syntacticValidate(this)

  def rawValidate(implicit networkPrefix: NetworkPrefix): Try[Unit] =
    TransferTransaction.syntacticValidate(this, hasAttMap = false)

}

object TransferTransaction {

  case class BoxParams[T <: TokenValueHolder](evidence: Evidence, nonce: Box.Nonce, value: T)

  /** Computes a unique nonce value based on the transaction type and
    * inputs and returns the details needed to create the output boxes for the transaction
    */
  def boxParams[
    T <: TokenValueHolder,
    P <: Proposition
  ](tx: TransferTransaction[T, P]): (BoxParams[SimpleValue], Traversable[BoxParams[T]]) = {
    // known input data (similar to messageToSign but without newBoxes since they aren't known yet)
    val inputBytes =
      Array(Transaction.identifier(tx).typePrefix) ++
      tx.boxIdsToOpen.foldLeft(Array[Byte]())((acc, x) => acc ++ x.hashBytes) ++
      Longs.toByteArray(tx.timestamp) ++
      tx.fee.toByteArray

    def calcNonce(index: Int): Box.Nonce = {
      val digest = Hash[Blake2b256, Digest32](inputBytes ++ Ints.toByteArray(index))
      Transaction.nonceFromDigest(digest)
    }

    val feeChangeParams = BoxParams(tx.to.head._1.evidence, calcNonce(0), SimpleValue(tx.to.head._2.quantity))

    val outputParams = tx.to.tail
      .filter(_._2.quantity > 0L)
      .zipWithIndex
      .map { case ((addr, value), idx) =>
        BoxParams(addr.evidence, calcNonce(idx + 1), value)
      }

    (feeChangeParams, outputParams)
  }

  def encodeFrom(from: IndexedSeq[(Address, Box.Nonce)]): Json = {
    from.map(x => (x._1.asJson, x._2.toString.asJson)).asJson
  }

  /** Retrieves the boxes from state for the specified sequence of senders and filters them based on the type of transaction */
  private def getSenderBoxesForTx(
    boxReader: BoxReader[ProgramId, Address],
    sender:    IndexedSeq[Address],
    txType:    String,
    assetArgs: Option[(AssetCode, Boolean)] = None
  ): Map[String, IndexedSeq[(String, Address, TokenBox[TokenValueHolder])]] =
    sender
      .flatMap { s =>
        boxReader
          .getTokenBoxes(s)
          .getOrElse(
            throw new Exception("No boxes found to fund transaction")
          ) // isn't this just an empty sequence instead of None?
          .collect {
            // always get polys because this is how fees are paid
            case bx: PolyBox =>
              ("Poly", s, bx)

            case bx: ArbitBox if txType == "ArbitTransfer" =>
              ("Arbit", s, bx)

            case bx: AssetBox if txType == "AssetTransfer" && assetArgs.forall(_._1 == bx.value.assetCode) =>
              ("Asset", s, bx)
          }
      }
      .groupBy(_._1)

  /** Determines the input boxes needed to create a transfer transaction
    *
    * @param boxReader a read-only version of the nodes current state
    * @param toReceive the recipients of boxes
    * @param sender the set of addresses that will contribute boxes to this transaction
    * @param fee the fee to be paid for the transaction
    * @param txType the type of transfer
    * @param assetArgs a tuple of asset specific details for finding the right asset boxes to be sent in a transfer
    * @return the input box information and output data needed to create the transaction case class
    */
  def createRawTransferParams[
    T <: TokenValueHolder
  ](
    boxReader:            BoxReader[ProgramId, Address],
    toReceive:            IndexedSeq[(Address, T)],
    sender:               IndexedSeq[Address],
    changeAddress:        Address,
    consolidationAddress: Option[Address],
    fee:                  Int128,
    txType:               String,
    assetArgs:            Option[(AssetCode, Boolean)] = None // (assetCode, minting)
  ): Try[(IndexedSeq[(Address, Box.Nonce)], IndexedSeq[(Address, TokenValueHolder)])] = Try {

    // Lookup boxes for the given senders
    val senderBoxes = getSenderBoxesForTx(boxReader, sender, txType, assetArgs)

    // compute the Poly balance since it is used often
    val polyBalance =
      senderBoxes
        .getOrElse("Poly", throw new Exception(s"No Poly funds available for the transaction fee payment"))
        .map(_._3.value.quantity)
        .sum

    // compute the amount of tokens that will be sent to the recipients
    val amtToSpend = toReceive.map(_._2.quantity).sum

    // ensure there are enough polys to pay the fee
    require(polyBalance >= fee, s"Insufficient funds available to pay transaction fee.")

    // create the list of inputs and outputs (senderChangeOut & recipientOut)
    val (availableToSpend, inputs, outputs) = txType match {
      case "PolyTransfer" =>
        (
          polyBalance - fee,
          senderBoxes("Poly").map(bxs => (bxs._2, bxs._3.nonce)),
          (changeAddress, SimpleValue(polyBalance - fee - amtToSpend)) +: toReceive
        )

      case "ArbitTransfer" =>
        val arbitBalance =
          senderBoxes
            .getOrElse("Arbit", throw new Exception(s"No Arbit funds available for the transaction"))
            .map(_._3.value.quantity)
            .sum

        (
          arbitBalance,
          senderBoxes("Arbit").map(bxs => (bxs._2, bxs._3.nonce)) ++
          senderBoxes("Poly").map(bxs => (bxs._2, bxs._3.nonce)),
          IndexedSeq(
            (changeAddress, SimpleValue(polyBalance - fee)),
            (consolidationAddress.getOrElse(changeAddress), SimpleValue(arbitBalance - amtToSpend))
          ) ++
          toReceive
        )

      // case for minting asset transfers
      // todo - JAA - what happens here when I specify a zero fee and use the same timestamp?
      // need to check that unique outputs are generated but I am not sure they will be because the tx
      // bytes will be the same so the nonce will end up being the same?
      case "AssetTransfer" if assetArgs.forall(_._2) =>
        (
          Int128.MaxValue,
          senderBoxes("Poly").map(bxs => (bxs._2, bxs._3.nonce)),
          (changeAddress, SimpleValue(polyBalance - fee)) +: toReceive
        )

      // todo: JAA - we need to handle the case where the change output is zero. I think this
      // means we should move these functions to their singleton objects and define the handling there
      case "AssetTransfer" =>
        val assetBalance =
          senderBoxes
            .getOrElse("Asset", throw new Exception(s"No Assets found with assetCode ${assetArgs.get._1}"))
            .map(_._3.value.quantity)
            .sum

        (
          assetBalance,
          senderBoxes("Asset").map(bxs => (bxs._2, bxs._3.nonce)) ++
          senderBoxes("Poly").map(bxs => (bxs._2, bxs._3.nonce)),
          IndexedSeq(
            (changeAddress, SimpleValue(polyBalance - fee)),
            (consolidationAddress.getOrElse(changeAddress), AssetValue(assetBalance - amtToSpend, assetArgs.get._1))
          ) ++
          toReceive
        )
    }

    // ensure there are sufficient funds from the sender boxes to create all outputs
    require(availableToSpend >= amtToSpend, "Insufficient funds available to create transaction.")

    (inputs, outputs)
  }

  /** Syntactic validation of a transfer transaction
    *
    * @param tx an instance of a transaction to check
    * @param hasAttMap boolean flag controlling whether signature verification should be checked or skipped
    * @return success or failure indicating the validity of the transaction
    */
  def syntacticValidate[
    T <: TokenValueHolder,
    P <: Proposition: EvidenceProducer
  ](tx: TransferTransaction[T, P], hasAttMap: Boolean = true)(implicit networkPrefix: NetworkPrefix): Try[Unit] = Try {

    // enforce transaction specific requirements
    tx match {
      case t: ArbitTransfer[_] if t.minting => // Arbit block rewards
      case t: PolyTransfer[_] if t.minting  => // Poly block rewards
      case t @ _                            =>
        // must provide input state to consume in order to generate new state
        if (t.minting) require(t.fee > 0L, "Asset minting transactions must have a non-zero positive fee")
        else require(t.fee >= 0L, "Transfer transactions must have a non-negative fee")

        require(t.from.nonEmpty, "Non-block reward transactions must specify at least one input box")
        require(t.to.forall(_._2.quantity > 0L), "Amount sent must be greater than 0")
    }

    require(tx.timestamp >= 0L, "Invalid timestamp")
    require(
      tx.data.forall(_.getValidLatin1Bytes.getOrElse(throw new Exception("String is not valid Latin-1")).length <= 128),
      "Data field must be less than 128 bytes"
    )

    // prototype transactions do not contain signatures at creation
    if (hasAttMap) {
      // ensure that the signatures are valid signatures with the body of the transaction
      require(
        tx.attestation.forall { case (prop, proof) =>
          proof.isValid(prop, tx.messageToSign)
        },
        "The provided proposition is not satisfied by the given proof"
      )

      // ensure that the propositions match the from addresses
      require(
        tx.from.forall { case (addr, _) =>
          tx.attestation.keys.map(_.generateEvidence).toSeq.contains(addr.evidence)
        },
        "The proposition(s) given do not match the evidence contained in the input boxes"
      )

      tx match {
        // ensure that the asset issuer is signing a minting transaction
        case t: AssetTransfer[_] if tx.minting =>
          t.to.foreach {
            case (_, asset: AssetValue) =>
              require(
                t.attestation.keys.map(_.address).toSeq.contains(asset.assetCode.issuer),
                "Asset minting must include the issuers signature"
              )
            // do nothing with other token types
            case (_, value: SimpleValue) =>
            case _                       => throw new Error("AssetTransfer contains invalid value holder")
          }

        case _ => // put additional checks on attestations here
      }
    }

    // ensure that the input and output lists of box ids are unique
    require(
      tx.newBoxes.forall(b ⇒ !tx.boxIdsToOpen.contains(b.id)),
      "The set of input box ids contains one or more of the output ids"
    )
  }

  /** Checks the stateful validity of a transaction
    *
    * @param tx the transaction to check
    * @param boxReader the state to check the validity against
    * @return a success or failure denoting the result of this check
    */
  def semanticValidate[
    T <: TokenValueHolder,
    P <: Proposition: EvidenceProducer
  ](tx:            TransferTransaction[T, P], boxReader: BoxReader[ProgramId, Address])(implicit
    networkPrefix: NetworkPrefix
  ): Try[Unit] = {

    // check that the transaction is correctly formed before checking state
    syntacticValidate(tx) match {
      case Failure(e) => throw e
      case _          => // continue processing
    }

    // compute transaction values used for validation
    val txOutput = tx.newBoxes.map(b => b.value.quantity).sum
    val unlockers = BoxUnlocker.generate(tx.from, tx.attestation)

    // iterate through the unlockers and sum up the value of the box for each valid unlocker
    unlockers
      .foldLeft[Try[Int128]](Success[Int128](0)) { (trySum, unlocker) =>
        trySum.flatMap { partialSum =>
          boxReader.getBox(unlocker.closedBoxId) match {
            case Some(box: TokenBox[_]) if unlocker.boxKey.isValid(unlocker.proposition, tx.messageToSign) =>
              Success(partialSum + box.value.quantity)

            case Some(_) => Failure(new Exception("Invalid unlocker"))
            case None    => Failure(new Exception(s"Box for unlocker $unlocker cannot be found in state"))
            case _       => Failure(new Exception("Invalid Box type for this transaction"))
          }
        }
      } match {
      // a normal transfer will fall in this case
      case Success(sum: Int128) if txOutput == sum - tx.fee =>
        Success(())

      // a minting transaction (of either Arbit, Polys, or Assets) will fall in this case
      case Success(_: Int128) if tx.minting =>
        Success(())

      case Success(sum: Int128) if !tx.minting && txOutput != sum - tx.fee =>
        Failure(
          new Exception(
            s"Tx output value does not equal input value for non-minting transaction. $txOutput != ${sum - tx.fee}"
          )
        )

      case Failure(e) => Failure(e)
    }
  }
}
