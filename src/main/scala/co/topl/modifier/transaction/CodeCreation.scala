package co.topl.modifier.transaction

import java.time.Instant

import co.topl.crypto.{FastCryptographicHash, PrivateKey25519, Signature25519}
import co.topl.modifier.transaction.Transaction.Nonce
import co.topl.modifier.transaction.serialization.CodeBoxCreationSerializer
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.nodeView.state.box.{Box, CodeBox}
import co.topl.nodeView.state.{ProgramId, StateReader}
import co.topl.program.ProgramPreprocessor
import co.topl.utils.serialization.BifrostSerializer
import co.topl.wallet.Wallet
import com.google.common.primitives.{Bytes, Longs}
import io.circe.syntax._
import io.circe.{Json, JsonObject}
import scorex.crypto.signatures.Signature
import scorex.util.encode.Base58

import scala.util.{Failure, Success, Try}

case class CodeCreation(to: PublicKey25519Proposition,
                        signature: Signature25519,
                        code: String,
                        override val fee: Long,
                        override val timestamp: Long,
                        data: String) extends Transaction {

  override type M = CodeCreation

  lazy val serializer: BifrostSerializer[CodeCreation] = CodeBoxCreationSerializer

  override lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq()

  lazy val hashNoNonces: Array[Byte] = FastCryptographicHash(
    to.pubKeyBytes ++
    code.getBytes ++
    Longs.toByteArray(fee) ++
    Longs.toByteArray(timestamp)
  )

  override val newBoxes: Traversable[Box] = {

    val nonceGen = FastCryptographicHash(
      "CodeCreation".getBytes ++
        to.pubKeyBytes ++
        code.getBytes ++
        hashNoNonces
      )

    val nonce = CodeCreation.nonceFromDigest(nonceGen)

    val programId = ProgramId.create(nonceGen ++ "programId".getBytes)

    val interface = ProgramPreprocessor("code", code)(JsonObject.empty).interface

    Seq(CodeBox(to, nonce, programId, Seq(code), interface))
  }

  override lazy val json: Json = Map(
    "txHash" -> id.toString.asJson,
    "txType" -> "CodeCreation".asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).toSeq.asJson,
    "to" -> Base58.encode(to.pubKeyBytes).asJson,
    "signature" -> Base58.encode(signature.signature).asJson,
    "code" -> code.asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson,
    "data" -> data.asJson
  ).asJson

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    "CodeCreation".getBytes,
    to.pubKeyBytes,
    newBoxes.foldLeft(Array[Byte]())((a, b) => a ++ b.bytes),
    code.getBytes,
    Longs.toByteArray(fee),
    data.getBytes
  )

  override def toString: String = s"CodeCreation(${json.noSpaces})"
}

object CodeCreation {

  type SR = StateReader[Box]

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  def createAndApply(w: Wallet,
                     to: PublicKey25519Proposition,
                     code: String,
                     fee: Long,
                     data: String): Try[CodeCreation] = Try {

    val selectedSecret = w.secretByPublicImage(to).get
    val fakeSig = Signature25519(Signature @@ Array.emptyByteArray)
    val timestamp = Instant.now.toEpochMilli
    val unsignedTx = CodeCreation(to, fakeSig, code, fee, timestamp, data)

    val signature = PrivateKey25519.sign(selectedSecret, unsignedTx.messageToSign)

    unsignedTx.copy(signature = signature)
  }

  def syntacticValidate(tx: CodeCreation, withSigs: Boolean = true): Try[Unit] = Try {
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
    require(tx.signature.isValid(tx.to, tx.messageToSign), "Invalid signature")

    tx.newBoxes.size match {
      //only one box should be created
      case 1 if (tx.newBoxes.head.isInstanceOf[CodeBox]) => Success(Unit)
      case _ => Failure(new Exception("Invlid transaction"))
    }
  }

  def validatePrototype(tx: CodeCreation): Try[Unit] = syntacticValidate(tx, withSigs = false)

  /**
    * Check the code is valid chain code and the newly created CodeBox is
    * formed properly
    *
    * @param tx : CodeCreation transaction
    * @return
    */
  def semanticValidate(tx: CodeCreation, state: SR): Try[Unit] = {

    // check that the transaction is correctly formed before checking state
    syntacticValidate(tx)

  }
}
