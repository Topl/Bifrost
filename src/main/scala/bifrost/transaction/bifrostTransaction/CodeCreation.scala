package bifrost.transaction.bifrostTransaction

import java.time.Instant
import java.util.UUID

import bifrost.crypto.hash.FastCryptographicHash
import bifrost.program.ProgramPreprocessor
import bifrost.transaction.bifrostTransaction.BifrostTransaction.Nonce
import bifrost.transaction.box.{BifrostBox, CodeBox}
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.serialization.CodeBoxCreationCompanion
import bifrost.transaction.state.PrivateKey25519Companion
import bifrost.wallet.BWallet
import com.google.common.primitives.{Bytes, Longs}
import io.circe.{Json, JsonObject}
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.util.Try

case class CodeCreation(to: PublicKey25519Proposition,
                        signature: Signature25519,
                        code: String,
                        override val fee: Long,
                        override val timestamp: Long,
                        data: String) extends BifrostTransaction {

  override type M = CodeCreation

  lazy val serializer = CodeBoxCreationCompanion

  override def toString: String = s"CodeCreation(${json.noSpaces})"

  override lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq()

  lazy val hashNoNonces: Array[Byte] = FastCryptographicHash(
    to.pubKeyBytes ++
      code.getBytes ++
      Longs.toByteArray(fee) ++
      Longs.toByteArray(timestamp)
  )

  override val newBoxes: Traversable[BifrostBox] = {

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
    "txHash" -> Base58.encode(id).asJson,
    "txType" -> "CodeCreation".asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).asJson,
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

  def createAndApply(w: BWallet,
                     to: PublicKey25519Proposition,
                     code: String,
                     fee: Long,
                     data: String): Try[CodeCreation] = Try {

    val selectedSecret = w.secretByPublicImage(to).get
    val fakeSig = Signature25519(Array())
    val timestamp = Instant.now.toEpochMilli
    val messageToSign = CodeCreation(to, fakeSig, code, fee, timestamp, data).messageToSign

    val signature = PrivateKey25519Companion.sign(selectedSecret, messageToSign)

    CodeCreation(to, signature, code, fee, timestamp, data)
  }
}
