package bifrost.transaction.bifrostTransaction

import java.time.Instant

import bifrost.crypto.hash.FastCryptographicHash
import BifrostTransaction.{Nonce, Value}
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.box.{AssetBox, BifrostBox}
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.serialization.AssetTransferCompanion
import bifrost.transaction.state.PrivateKey25519
import bifrost.wallet.BWallet
import com.google.common.primitives.{Bytes, Ints}
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.util.Try

case class AssetTransfer(override val from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                         override val to: IndexedSeq[(PublicKey25519Proposition, Long)],
                         override val signatures: IndexedSeq[Signature25519],
                         issuer: PublicKey25519Proposition,
                         assetCode: String,
                         override val fee: Long,
                         override val timestamp: Long,
                         val data: String)
  extends TransferTransaction(from, to, signatures, fee, timestamp) {

  override type M = AssetTransfer

  override lazy val serializer = AssetTransferCompanion

  override def toString: String = s"AssetTransfer(${json.noSpaces})"

  override lazy val newBoxes: Traversable[BifrostBox] = to.zipWithIndex.map {
    case ((prop, value), idx) =>
      val nonce = AssetTransfer.nonceFromDigest(FastCryptographicHash(
        "AssetTransfer".getBytes ++
          prop.pubKeyBytes ++
          issuer.pubKeyBytes ++
          assetCode.getBytes ++
          hashNoNonces ++
          Ints.toByteArray(idx)
      ))
      AssetBox(prop, nonce, value, assetCode, issuer, data)
  }

  override lazy val json: Json = Map(
    "txHash" -> Base58.encode(id).asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).asJson,
    "boxesToRemove" -> boxIdsToOpen.map(id => Base58.encode(id).asJson).asJson,
    "from" -> from.map { s =>
      Map(
        "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
        "nonce" -> s._2.asJson
      ).asJson
    }.asJson,
    "to" -> to.map { s =>
      Map(
        "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
        "value" -> s._2.asJson
      ).asJson
    }.asJson,
    "issuer" -> Base58.encode(issuer.pubKeyBytes).asJson,
    "assetCode" -> assetCode.asJson,
    "signatures" -> signatures.map(s => Base58.encode(s.signature).asJson).asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson,
    "data" -> data.asJson
  ).asJson

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    "AssetTransfer".getBytes(),
    super.commonMessageToSign,
    issuer.pubKeyBytes,
    assetCode.getBytes,
    data.getBytes
  )
}

object AssetTransfer extends TransferUtil {

  def apply(from: IndexedSeq[(PrivateKey25519, Nonce)],
            to: IndexedSeq[(PublicKey25519Proposition, Value)],
            issuer: PublicKey25519Proposition,
            assetCode: String,
            fee: Long,
            timestamp: Long,
            data: String): AssetTransfer = {
    val params = parametersForApply(from, to, fee, timestamp, "AssetTransfer", issuer, assetCode, data).get
    AssetTransfer(params._1, to, params._2, issuer, assetCode, fee, timestamp, data)
  }

  def create(w: BWallet,
             toReceive: IndexedSeq[(PublicKey25519Proposition, Long)],
             fee: Long,
             issuer: PublicKey25519Proposition,
             assetCode: String,
             data: String,
             publicKeyToSendFrom: Vector[String] = Vector(),
             publicKeyToSendChangeTo: String = ""): Try[AssetTransfer] = Try {

    val params = parametersForCreate(w, toReceive, fee, "AssetTransfer", publicKeyToSendFrom, publicKeyToSendChangeTo, issuer, assetCode)
    val timestamp = Instant.now.toEpochMilli
    AssetTransfer(params._1.map(t => t._1 -> t._2), params._2, issuer, assetCode, fee, timestamp, data)
  }


  def validate(tx: AssetTransfer): Try[Unit] = validateTx(tx)
}