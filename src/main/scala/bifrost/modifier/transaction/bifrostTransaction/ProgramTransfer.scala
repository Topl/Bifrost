package bifrost.modifier.transaction.bifrostTransaction

import java.time.Instant

import bifrost.crypto.{ FastCryptographicHash, PrivateKey25519, PrivateKey25519Companion, Signature25519 }
import bifrost.modifier.box.proposition.{ ProofOfKnowledgeProposition, PublicKey25519Proposition }
import bifrost.modifier.box.{ Box, ExecutionBox }
import bifrost.modifier.transaction.bifrostTransaction.Transaction.Nonce
import bifrost.modifier.transaction.serialization.ProgramTransferSerializer
import bifrost.state.{ State, StateReader }
import bifrost.utils.serialization.BifrostSerializer
import bifrost.wallet.Wallet
import com.google.common.primitives.{ Bytes, Longs }
import io.circe.Json
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import scorex.crypto.encode.Base58

import scala.util.{ Failure, Success, Try }

case class ProgramTransfer ( from        : PublicKey25519Proposition,
                             to          : PublicKey25519Proposition,
                             signature   : Signature25519,
                             executionBox: ExecutionBox,
                             fee         : Long,
                             timestamp   : Long,
                             data        : String
                           ) extends Transaction {

  override type M = ProgramTransfer

  override lazy val serializer: BifrostSerializer[ProgramTransfer] = ProgramTransferSerializer

  lazy val hashNoNonces: Array[Byte] = FastCryptographicHash(
    to.pubKeyBytes
      ++ Longs.toByteArray(fee)
      ++ data.getBytes
    )

  override lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq(executionBox.id)

  override lazy val newBoxes: Traversable[Box] = {

    val nonce = ProgramTransfer.nonceFromDigest(
      FastCryptographicHash("ProgramTransfer".getBytes
                              ++ to.pubKeyBytes
                              ++ hashNoNonces
                            ))

    Seq(ExecutionBox(to, nonce, executionBox.value, executionBox.stateBoxIds, executionBox.codeBoxIds))
  }

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    "ProgramTransfer".getBytes
      ++ to.pubKeyBytes
      ++ newBoxes.head.bytes
      ++ Longs.toByteArray(fee)
    )

  override lazy val json: Json = Map(
    "txHash" -> id.toString.asJson,
    "txType" -> "ProgramTransfer".asJson,
    "newBoxes" -> Base58.encode(newBoxes.head.id).asJson,
    "boxesToRemove" -> Base58.encode(boxIdsToOpen.head).asJson,
    "from" -> Base58.encode(from.pubKeyBytes).asJson,
    "to" -> Base58.encode(to.pubKeyBytes).asJson,
    "signature" -> Base58.encode(signature.signature).asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson,
    "data" -> data.asJson
    ).asJson

  override def toString: String = s"ProgramTransfer(${json.noSpaces})"
}

object ProgramTransfer {

  type SR = StateReader[Box, ProofOfKnowledgeProposition[PrivateKey25519], Any]

  def nonceFromDigest ( digest: Array[Byte] ): Nonce = Longs.fromByteArray(digest.take(8))

  def createAndApply ( w           : Wallet,
                       from        : PublicKey25519Proposition,
                       to          : PublicKey25519Proposition,
                       executionBox: ExecutionBox,
                       fee         : Long,
                       data        : String
                     ): Try[ProgramTransfer] = Try {

    val selectedSecret = w.secretByPublicImage(from).get
    val fakeSig = Signature25519(Array())
    val timestamp = Instant.now.toEpochMilli
    val messageToSign = ProgramTransfer(from, to, fakeSig, executionBox, fee, timestamp, data).messageToSign

    val signature = PrivateKey25519Companion.sign(selectedSecret, messageToSign)

    ProgramTransfer(from, to, signature, executionBox, fee, timestamp, data)
  }

  //TODO implement prototype tx
  /*def createPrototype(tokenBoxRegistry: TokenBoxRegistry,
                      toReceive: IndexedSeq[(PublicKey25519Proposition, Long)],
                      sender: IndexedSeq[PublicKey25519Proposition],
                      fee: Long, data: String): Try[PolyTransfer] = Try
  {
    val params = parametersForCreate(tokenBoxRegistry, toReceive, sender, fee, "ProgramTransfer")
    val timestamp = Instant.now.toEpochMilli
    ProgramTransfer(params._1.map(t => t._1 -> t._2), params._2, Map(), fee, timestamp, data)
  }
   */

  def validatePrototype ( tx: ProgramTransfer ): Try[Unit] = syntacticValidate(tx, withSigs = false)

  def syntacticValidate ( tx: ProgramTransfer, withSigs: Boolean = true ): Try[Unit] = Try {
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)

    if ( withSigs ) {
      require(tx.signature.isValid(tx.from, tx.messageToSign))
    }

    // only allow a single box to be transferred
    tx.newBoxes.size match {
      case 1 if (tx.newBoxes.head.isInstanceOf[ExecutionBox]) => Success(Unit)
      case _                                                  => Failure(new Exception("Invalid transaction"))
    }

    // ensure unique list of inuts and output
    val wrappedBoxIdsToOpen = tx.boxIdsToOpen.map(b ⇒ ByteArrayWrapper(b))
    require(tx.newBoxes.forall(b ⇒ !wrappedBoxIdsToOpen.contains(ByteArrayWrapper(b.id))))
  }

  def semanticValidate ( tx: ProgramTransfer, state: SR ): Try[Unit] = {

    // check that the transaction is correctly formed before checking state
    syntacticValidate(tx) match {
      case Failure(e) => throw e
      case _          => // continue processing
    }

    val from = Seq((tx.from, tx.executionBox.nonce))
    val signature = Map(tx.from -> tx.signature)
    val unlocker = State.generateUnlockers(from, signature).head

    state.getBox(unlocker.closedBoxId) match {
      case Some(box: ExecutionBox) if unlocker.boxKey.isValid(box.proposition, tx.messageToSign) => Success(Unit)
      case Some(_)                                                                               => Failure(new Exception("Invalid unlocker"))
      case None                                                                                  => Failure(new Exception(s"Box for unlocker $unlocker cannot be found in state"))
      case _                                                                                     => Failure(new Exception("Invalid Box type for this transaction"))
    }
  }
}
