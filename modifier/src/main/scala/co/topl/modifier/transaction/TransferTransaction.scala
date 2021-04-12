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
import scorex.crypto.hash.Blake2b256

import scala.util.{Failure, Success, Try}

abstract class TransferTransaction[
  +T <: TokenValueHolder,
  P <: Proposition: EvidenceProducer: Identifiable
](
  val from:        IndexedSeq[(Address, Box.Nonce)],
  val to:          IndexedSeq[(Address, TokenValueHolder)],
  val attestation: Map[P, Proof[P]],
  val fee:         Int128,
  val timestamp:   Long,
  val data:        Option[String],
  val minting:     Boolean
) extends Transaction[T, P] {

  lazy val bloomTopics: IndexedSeq[BloomTopic] = to.map(b => BloomTopic @@ b._1.bytes)

  lazy val boxIdsToOpen: IndexedSeq[BoxId] = from.map { case (addr, nonce) =>
    BoxId.idFromEviNonce(addr.evidence, nonce)
  }

  val (feeOutputParams, coinOutputParams) = TransferTransaction.calculateBoxNonce[TokenValueHolder](this, to)

  val feeChangeOutput: PolyBox =
    PolyBox(feeOutputParams.evidence, feeOutputParams.nonce, feeOutputParams.value)

  val coinOutput: Traversable[TokenBox[T]]

  override val newBoxes: Traversable[TokenBox[T]]

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

  case class TransferCreationState(
    senderBoxes: Map[String, IndexedSeq[(String, Address, TokenBox[TokenValueHolder])]],
    polyBalance: Int128
  )

  def encodeFrom(from: IndexedSeq[(Address, Box.Nonce)]): Json =
    from.map(x => (x._1.asJson, x._2.toString.asJson)).asJson

  /** Computes a unique nonce value based on the transaction type and
    * inputs and returns the details needed to create the output boxes for the transaction
    */
  def calculateBoxNonce[T <: TokenValueHolder](
    tx: TransferTransaction[T, _ <: Proposition],
    to: IndexedSeq[(Address, T)]
  ): (BoxParams[SimpleValue], Traversable[BoxParams[T]]) = {

    // known input data (similar to messageToSign but without newBoxes since they aren't known yet)
    val txIdPrefix = Transaction.identifier(tx).typePrefix
    val boxIdsToOpenAccumulator = tx.boxIdsToOpen.foldLeft(Array[Byte]())((acc, x) => acc ++ x.hashBytes)
    val timestampBytes = Longs.toByteArray(tx.timestamp)
    val feeBytes = tx.fee.toByteArray

    val inputBytes =
      Array(txIdPrefix) ++ boxIdsToOpenAccumulator ++ timestampBytes ++ feeBytes

    val calcNonce: Int => Box.Nonce = (index: Int) => {
      val digest = Blake2b256(inputBytes ++ Ints.toByteArray(index))
      Transaction.nonceFromDigest(digest)
    }

    val feeChangeParams = BoxParams(tx.to.head._1.evidence, calcNonce(0), SimpleValue(tx.to.head._2.quantity))

    val coinOutputParams: IndexedSeq[BoxParams[T]] =
      to.tail.zipWithIndex
        .map { case ((addr, value), idx) =>
          BoxParams[T](addr.evidence, calcNonce(idx + 1), value)
        }

    (feeChangeParams, coinOutputParams)
  }

  /** Retrieves the boxes from state for the specified sequence of senders and filters them based on the type of transaction */
  private def getSenderBoxesForTx(
    boxReader:   BoxReader[ProgramId, Address],
    sender:      IndexedSeq[Address],
    returnBoxes: String,
    assetCode:   Option[AssetCode] = None
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

            case bx: ArbitBox if returnBoxes == "Arbits" =>
              ("Arbit", s, bx)

            case bx: AssetBox if returnBoxes == "Assets" && assetCode.forall(_ == bx.value.assetCode) =>
              ("Asset", s, bx)
          }
      }
      .groupBy(_._1)

  /** Determines the input boxes needed to create a transfer transaction
    *
    * @param boxReader a read-only version of the nodes current state
    * @param sender the set of addresses that will contribute boxes to this transaction
    * @param fee the fee to be paid for the transaction
    * @param txType the type of transfer
    * @param assetCode an asset specific detail for finding the right asset boxes to be sent in a transfer
    * @return the input box information and output data needed to create the transaction case class
    */
  def getSenderBoxesAndCheckPolyBalance(
    boxReader: BoxReader[ProgramId, Address],
    sender:    IndexedSeq[Address],
    fee:       Int128,
    txType:    String,
    assetCode: Option[AssetCode] = None // (assetCode)
  ): Try[TransferCreationState] = Try {

    // Lookup boxes for the given senders
    val senderBoxes: Map[String, IndexedSeq[(String, Address, TokenBox[TokenValueHolder])]] =
      getSenderBoxesForTx(boxReader, sender, txType, assetCode)

    // compute the Poly balance since it is used often
    val polyBalance =
      senderBoxes
        .getOrElse("Poly", throw new Exception(s"No Poly funds available for the transaction fee payment"))
        .map(_._3.value.quantity)
        .sum

    // ensure there are enough polys to pay the fee
    require(polyBalance >= fee, s"Insufficient funds available to pay transaction fee.")

    TransferCreationState(senderBoxes, polyBalance)
  }

  /** Syntactic validation of a transfer transaction
    *
    * @param tx an instance of a transaction to check
    * @param hasAttMap boolean flag controlling whether signature verification should be checked or skipped
    * @return success or failure indicating the validity of the transaction
    */
  // todo: JAA - this is the sort of transaction that should be validated, because I want to fix all of my error at once
  def syntacticValidate[
    T <: TokenValueHolder,
    P <: Proposition: EvidenceProducer
  ](tx: TransferTransaction[T, P], hasAttMap: Boolean = true)(implicit networkPrefix: NetworkPrefix): Try[Unit] = Try {

    // enforce transaction specific requirements
    tx match {
      case t: ArbitTransfer[_] if t.minting => // Arbit block rewards
      case t: PolyTransfer[_] if t.minting  => // Poly block rewards
      case t: PolyTransfer[_] =>
        require(t.from.nonEmpty, "Non-block reward transactions must specify at least one input box")

      case t: ArbitTransfer[_] =>
        require(
          tx.to.size >= 2,
          "ArbitTransfers must specify two or more recipients in the `to` value such as [change_output, coin_output(s)]"
        )
        require(t.from.nonEmpty, "Non-block reward transactions must specify at least one input box")

      case t: AssetTransfer[_] =>
        require(
          tx.to.size >= 2,
          "AssetTransfers must specify two or more recipients in the `to` value such as [change_output, coin_output(s)]"
        )
        require(t.from.nonEmpty, "Non-block reward transactions must specify at least one input box")
    }

    require(tx.fee >= 0, "Transfer transactions must have a non-negative fee")
    require(tx.timestamp >= 0L, "Invalid timestamp")
    require(
      tx.data.forall(_.getValidLatin1Bytes.getOrElse(throw new Exception("String is not valid Latin-1")).length <= 128),
      "Data field must be less than 128 bytes"
    )

    // ensure that the input and output lists of box ids are unique
    require(
      tx.newBoxes.forall(b ⇒ !tx.boxIdsToOpen.contains(b.id)),
      "The set of input box ids contains one or more of the output ids"
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
  }

  /** Checks the stateful validity of a transaction
    *
    * @param tx the transaction to check
    * @param boxReader the state to check the validity against
    * @return a success or failure denoting the result of this check
    */
  // todo: JAA - this is the sort of validation that should fail eagerly (since it can be expensive)
  def semanticValidate[
    T <: TokenValueHolder,
    P <: Proposition: EvidenceProducer
  ](tx:            TransferTransaction[T, P], boxReader: BoxReader[ProgramId, Address])(implicit
    networkPrefix: NetworkPrefix
  ): Try[Unit] = {

    // compute transaction values used for validation
    val txOutput = tx.newBoxes.map(b => b.value.quantity).sum
    val unlockers = BoxUnlocker.generate(tx.from, tx.attestation)

    val inputBoxes = unlockers.map { u =>
      u -> boxReader.getBox(u.closedBoxId)
    }

    val sumOfPolyInputs = inputBoxes.collect { case (_, Some(PolyBox(_, _, value))) =>
      value.quantity
    }.sum

    // check that the transaction is correctly formed before checking state
    lazy val syntacticResult = syntacticValidate(tx)

    // enforce transaction specific requirements
    // must provide input state to consume in order to generate new state
    lazy val txSpecific = Try {
      tx match {
        case t: PolyTransfer[_] if t.minting => // Poly block rewards (skip enfocring)
        case t: ArbitTransfer[_] if t.minting => // Arbit block rewards (skip enforcing)

        case t: PolyTransfer[_] =>
          require(
            sumOfPolyInputs - t.fee == txOutput,
            s"PolyTransfer output value does not equal input value for non-minting transaction. " +
              s"$txOutput != ${sumOfPolyInputs - t.fee}"
          )

        case t@_ =>
          require(
            sumOfPolyInputs - t.fee == t.feeChangeOutput.value.quantity,
            s"feeChangeOutput value does not equal input value for non-minting transaction. " +
              s"${t.feeChangeOutput.value.quantity} != ${sumOfPolyInputs - t.fee}"
          )
      }
    }

    // iterate through the unlockers and sum up the value of the box for each valid unlocker
    lazy val accessibleFunds = inputBoxes
      .foldLeft[Try[Int128]](Success[Int128](0)) { case (trySum, (unlocker, boxOpt)) =>
        trySum.flatMap { partialSum =>
          boxOpt match {
            case Some(box: TokenBox[_]) if unlocker.boxKey.isValid(unlocker.proposition, tx.messageToSign) =>
              Success(partialSum + box.value.quantity)

            case Some(_) => Failure(new Exception("Invalid unlocker"))
            case None => Failure(new Exception(s"Box for unlocker $unlocker cannot be found in state"))
            case _ => Failure(new Exception("Invalid Box type for this transaction"))
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

    for {
      _ <- syntacticResult
      _ <- txSpecific
      _ <- accessibleFunds
    } yield Success(())
  }
}
