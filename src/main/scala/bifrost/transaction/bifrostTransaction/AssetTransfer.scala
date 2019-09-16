package bifrost.transaction.bifrostTransaction

import java.time.Instant

import bifrost.crypto.hash.FastCryptographicHash
import BifrostTransaction.{Nonce, Value}
import bifrost.tokenBoxRegistry.TokenBoxRegistry
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
                         override val signatures: Map[PublicKey25519Proposition, Signature25519],
                         issuer: PublicKey25519Proposition,
                         assetCode: String,
                         override val fee: Long,
                         override val timestamp: Long,
                         override val data: String)
  extends TransferTransaction(from, to, signatures, fee, timestamp, data) {

  override type M = AssetTransfer

  override lazy val serializer = AssetTransferCompanion

  override def toString: String = s"AssetTransfer(${json.noSpaces})"

  override lazy val newBoxes: Traversable[BifrostBox] = to
    .filter(toInstance => toInstance._2 > 0L)
    .zipWithIndex
    .map {
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
    "txType" -> "AssetTransfer".asJson,
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
    "signatures" -> signatures
      .map { s =>
        Map(
          "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
          "signature" -> Base58.encode(s._2.signature).asJson
        ).asJson
      }.asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson,
    "data" -> data.asJson
  ).asJson


  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    "AssetTransfer".getBytes(),
    super.commonMessageToSign,
    issuer.pubKeyBytes,
    assetCode.getBytes,
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

  def create(tbr:TokenBoxRegistry,
             w: BWallet,
             toReceive: IndexedSeq[(PublicKey25519Proposition, Long)],
             sender: IndexedSeq[PublicKey25519Proposition],
             fee: Long,
             issuer: PublicKey25519Proposition,
             assetCode: String,
             data: String): Try[AssetTransfer] = Try {

    val params = parametersForCreate(tbr, w, toReceive, sender, fee, "AssetTransfer", issuer, assetCode)
    val timestamp = Instant.now.toEpochMilli
    AssetTransfer(params._1.map(t => t._1 -> t._2), params._2, issuer, assetCode, fee, timestamp, data)
  }

  def createPrototype(tbr: TokenBoxRegistry,
                      toReceive: IndexedSeq[(PublicKey25519Proposition, Long)],
                      sender: IndexedSeq[PublicKey25519Proposition],
                      issuer: PublicKey25519Proposition,
                      assetCode: String,
                      fee: Long,
                      data: String): Try[AssetTransfer] = Try
  {
    val params = parametersForCreate(tbr, toReceive, sender, fee, "AssetTransfer", issuer, assetCode)
    val timestamp = Instant.now.toEpochMilli
    AssetTransfer(params._1.map(t => t._1 -> t._2), params._2, Map(), issuer, assetCode, fee, timestamp, data)
  }

  def validate(tx: AssetTransfer): Try[Unit] = validateTx(tx)

  def validatePrototype(tx: AssetTransfer): Try[Unit] = validateTxWithoutSignatures(tx)

}