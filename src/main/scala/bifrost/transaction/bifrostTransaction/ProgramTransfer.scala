package bifrost.transaction.bifrostTransaction

import java.util.UUID

import bifrost.crypto.hash.FastCryptographicHash
import bifrost.serialization.Serializer
import bifrost.transaction.account.PublicKeyNoncedBox
import bifrost.transaction.bifrostTransaction.BifrostTransaction.Nonce
import bifrost.transaction.box.{BifrostBox, BoxUnlocker, ExecutionBox}
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.serialization.ProgramTransferCompanion
import com.google.common.primitives.{Bytes, Longs}
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

case class ProgramTransfer(from: (PublicKey25519Proposition, Nonce),
                           to: PublicKey25519Proposition,
                           signature: Signature25519,
                           executionBox: ExecutionBox,
                           fee: Long,
                           timestamp: Long,
                           data: String) extends BifrostTransaction {

  override type M = ProgramTransfer

  override lazy val serializer: Serializer[ProgramTransfer] = ProgramTransferCompanion

  override def toString: String = s"ProgramTransfer(${json.noSpaces})"

  lazy val hashNoNonces: Array[Byte] = FastCryptographicHash(
    to.pubKeyBytes
      ++ Longs.toByteArray(fee)
      ++ data.getBytes
  )

  override lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq(PublicKeyNoncedBox.idFromBox(from._1, from._2))

  override lazy val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] =
    if(signature.isInstanceOf[Signature25519])
    Seq(new BoxUnlocker[PublicKey25519Proposition] {
      override val closedBoxId: Array[Byte] = PublicKeyNoncedBox.idFromBox(from._1, from._2)
      override val boxKey: Signature25519 = signature
    })
    else Traversable()

  override lazy val newBoxes: Traversable[BifrostBox] = {

    val nonce = ProgramTransfer.nonceFromDigest(
      FastCryptographicHash("ProgramTransfer".getBytes
        ++ to.pubKeyBytes
        ++ hashNoNonces
      ))

    val uuid = UUID.nameUUIDFromBytes(ExecutionBox.idFromBox(to, nonce))

    Seq(ExecutionBox(to, nonce, uuid, executionBox.stateBoxUUIDs, executionBox.codeBoxIds))
  }

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    "ProgramTransfer".getBytes
      ++ to.pubKeyBytes
      ++ newBoxes.head.bytes
      ++ Longs.toByteArray(fee)
  )

  override lazy val json: Json = Map(
    "txHash" -> Base58.encode(id).asJson,
    "txType" -> "ProgramTransfer".asJson,
    "newBoxes" -> Base58.encode(newBoxes.head.id).asJson,
    "boxesToRemove" -> Base58.encode(boxIdsToOpen.head).asJson,
    "from" ->
      Map(
        "proposition" -> Base58.encode(from._1.pubKeyBytes).asJson,
        "nonce" -> from._2.asJson,
      ).asJson,
    "to" -> Base58.encode(to.pubKeyBytes).asJson,
    "signature" -> Base58.encode(signature.signature).asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson,
    "data" -> data.asJson
  ).asJson
}

object ProgramTransfer {

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(8))

  //TODO implement create and apply for ProgramTransfer
  /*def apply(from: IndexedSeq[(PrivateKey25519, Nonce)],
            to: IndexedSeq[(PublicKey25519Proposition, Value)],
            fee: Long,
            timestamp: Long,
            data: String): ProgramTransfer = {
    val params = parametersForApply(from, to, fee, timestamp, "ProgramTransfer", data).get
    ProgramTransfer(params._1.head, to, params._2, fee, timestamp, data)
  }

  def create(bfr: BFR,
             w: BWallet,
             toReceive: IndexedSeq[(PublicKey25519Proposition, Long)],
             sender: IndexedSeq[PublicKey25519Proposition],
             fee: Long, data: String): Try[ProgramTransfer] = Try {
    val params = parametersForCreate(bfr, w, toReceive, sender, fee, "ProgramTransfer")
    val timestamp = Instant.now.toEpochMilli
    ProgramTransfer(params._1.map(t => t._1 -> t._2), params._2, fee, timestamp, data)
  }
   */

  //TODO implement prototype tx
  /*def createPrototype(bfr: BFR, toReceive: IndexedSeq[(PublicKey25519Proposition, Long)], sender: IndexedSeq[PublicKey25519Proposition], fee: Long, data: String): Try[PolyTransfer] = Try
  {
    val params = parametersForCreate(bfr, toReceive, sender, fee, "ProgramTransfer")
    val timestamp = Instant.now.toEpochMilli
    ProgramTransfer(params._1.map(t => t._1 -> t._2), params._2, Map(), fee, timestamp, data)
  }
   */

  //def validate(tx: PolyTransfer): Try[Unit] = validateTx(tx)

  //def validatePrototype(tx: PolyTransfer): Try[Unit] = validateTxWithoutSignatures(tx)
}
