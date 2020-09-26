package bifrost.modifier.transaction.bifrostTransaction

import java.time.Instant
import java.util.UUID

import bifrost.crypto.{FastCryptographicHash, PrivateKey25519Companion, Signature25519}
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.box.{Box, CodeBox}
import bifrost.modifier.transaction.bifrostTransaction.Transaction.Nonce
import bifrost.modifier.transaction.serialization.CodeBoxCreationSerializer
import bifrost.program.ProgramPreprocessor
import bifrost.utils.serialization.BifrostSerializer
import bifrost.wallet.Wallet
import com.google.common.primitives.{Bytes, Longs}
import io.circe.syntax._
import io.circe.{Json, JsonObject}
import scorex.util.encode.Base58
import scorex.crypto.signatures.Signature

import scala.util.Try

case class CodeCreation(to: PublicKey25519Proposition,
                        signature: Signature25519,
                        code: String,
                        override val fee: Long,
                        override val timestamp: Long,
                        data: String) extends Transaction {

  override type M = CodeCreation

  lazy val serializer: BifrostSerializer[CodeCreation] = CodeBoxCreationSerializer

  override def toString: String = s"CodeCreation(${json.noSpaces})"

  override lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq()

  lazy val hashNoNonces: Array[Byte] = FastCryptographicHash(
    to.pubKeyBytes ++
      code.getBytes ++
      Longs.toByteArray(fee) ++
      Longs.toByteArray(timestamp)
  )

  override val newBoxes: Traversable[Box] = {

    val nonce = CodeCreation.nonceFromDigest(FastCryptographicHash(
      "CodeCreation".getBytes ++
        to.pubKeyBytes ++
        code.getBytes ++
        hashNoNonces
    ))

    val uuid = UUID.nameUUIDFromBytes(CodeBox.idFromBox(to, nonce))

    val interface = ProgramPreprocessor("code", code)(JsonObject.empty).interface

    Seq(CodeBox(to, nonce, uuid, Seq(code), interface))
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
}

object CodeCreation {

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  def validate(tx: CodeCreation): Try[Unit] = Try {
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
    require(tx.signature.isValid(tx.to, tx.messageToSign), "Invalid signature")
  }

  def createAndApply(w: Wallet,
                     to: PublicKey25519Proposition,
                     code: String,
                     fee: Long,
                     data: String): Try[CodeCreation] = Try {

    val selectedSecret = w.secretByPublicImage(to).get
    val fakeSig = Signature25519(Signature @@ Array.emptyByteArray)
    val timestamp = Instant.now.toEpochMilli
    val messageToSign = CodeCreation(to, fakeSig, code, fee, timestamp, data).messageToSign

    val signature = PrivateKey25519Companion.sign(selectedSecret, messageToSign)

    CodeCreation(to, signature, code, fee, timestamp, data)
  }
}
