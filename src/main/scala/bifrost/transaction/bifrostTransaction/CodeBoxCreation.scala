package bifrost.transaction.bifrostTransaction

import java.util.UUID

import bifrost.crypto.hash.FastCryptographicHash
import bifrost.program.ProgramPreprocessor
import bifrost.transaction.bifrostTransaction.BifrostTransaction.Nonce
import bifrost.transaction.box.{BifrostBox, BoxUnlocker, CodeBox}
import bifrost.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.serialization.CodeBoxCreationCompanion
import bifrost.transaction.state.PrivateKey25519
import com.google.common.primitives.{Bytes, Longs}
import io.circe.{Json, JsonObject}
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.util.Try

case class CodeBoxCreation(to: PublicKey25519Proposition,
                           signature: Signature25519,
                           code: String,
                           override val fee: Long,
                           override val timestamp: Long,
                           data: String) extends BifrostTransaction {

  override type M = CodeBoxCreation

  lazy val serializer = CodeBoxCreationCompanion

  override def toString: String = s"CodeBoxCreation(${json.noSpaces})"

  override lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq()

  override lazy val unlockers: Traversable[BoxUnlocker[ProofOfKnowledgeProposition[PrivateKey25519]]] = Traversable()

  lazy val hashNoNonces = FastCryptographicHash(
    to.pubKeyBytes ++
      code.getBytes ++
      Longs.toByteArray(fee) ++
      Longs.toByteArray(timestamp)
  )

  override val newBoxes: Traversable[BifrostBox] = {

    val nonce = CodeBoxCreation.nonceFromDigest(FastCryptographicHash(
      "CodeBoxCreation".getBytes ++
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
    "txType" -> "CodeBoxCreation".asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).asJson,
    "to" -> Base58.encode(to.pubKeyBytes).asJson,
    "signature" -> Base58.encode(signature.signature).asJson,
    "code" -> code.asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson,
    "data" -> data.asJson
  ).asJson

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    "CodeBoxCreation".getBytes,
    to.pubKeyBytes,
    newBoxes.foldLeft(Array[Byte]())((a, b) => a ++ b.bytes),
    code.getBytes,
    Longs.toByteArray(fee),
    data.getBytes
  )
}

object CodeBoxCreation {

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  def validate(tx: CodeBoxCreation): Try[Unit] = Try {
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
    require(tx.signature.isValid(tx.to, tx.messageToSign), "Invalid signature")
  }

  //TODO Create prototype tx
}
