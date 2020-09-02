package bifrost.modifier.transaction.bifrostTransaction

import java.time.Instant

import bifrost.crypto.{FastCryptographicHash, PrivateKey25519Companion, Signature25519}
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.box.{AssetBox, Box}
import bifrost.modifier.transaction.bifrostTransaction.Transaction.Nonce
import bifrost.modifier.transaction.serialization.AssetCreationSerializer
import bifrost.wallet.Wallet
import bifrost.utils.idToBytes
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.syntax._
import io.circe.{Decoder, HCursor, Json}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.util.Try

case class AssetCreation (to: IndexedSeq[(PublicKey25519Proposition, Long)],
                          signatures: Map[PublicKey25519Proposition, Signature25519],
                          assetCode: String,
                          issuer: PublicKey25519Proposition,
                          override val fee: Long,
                          override val timestamp: Long,
                          data: String) extends Transaction {


  override type M = AssetCreation

  lazy val serializer = AssetCreationSerializer

  override def toString: String = s"AssetCreation(${json.noSpaces})"

  override lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq()

  //TODO deprecate timestamp once fee boxes are included in nonce generation
  lazy val hashNoNonces = FastCryptographicHash(
    to.map(_._1.pubKeyBytes).reduce(_ ++ _) ++
    Longs.toByteArray(timestamp) ++
    Longs.toByteArray(fee)
  )

   override lazy val newBoxes: Traversable[Box] = to
     .filter(toInstance => toInstance._2 > 0L)
     .zipWithIndex
     .map {
     case ((prop, value), idx) =>
       val nonce = AssetCreation.nonceFromDigest(FastCryptographicHash(
         "AssetCreation".getBytes ++
           prop.pubKeyBytes ++
           issuer.pubKeyBytes ++
           assetCode.getBytes ++
           hashNoNonces ++
           Ints.toByteArray(idx)
       ))
       AssetBox(prop, nonce, value, assetCode, issuer, data)
     }

  override lazy val json: Json = Map(
    "txHash" -> Base58.encode(idToBytes(id)).asJson,
    "txType" -> "AssetCreation".asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).toSeq.asJson,
    "to" -> to.map { case (prop, value) =>
      Base58.encode(prop.pubKeyBytes) -> value.toString.asJson
    }.asJson,
    "issuer" -> Base58.encode(issuer.pubKeyBytes).asJson,
    "assetCode" -> assetCode.asJson,
    "signatures" -> signatures.map { case (prop, sig) =>
      Base58.encode(prop.pubKeyBytes) -> Base58.encode(sig.signature)
    }.asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson,
    "data" -> data.asJson
  ).asJson

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    "AssetCreation".getBytes(),
    to.map(_._1.pubKeyBytes).reduce(_ ++ _) ++
    newBoxes.foldLeft(Array[Byte]())((acc, x) => acc ++ x.bytes),
    issuer.pubKeyBytes,
    assetCode.getBytes,
    Longs.toByteArray(fee),
    data.getBytes
  )

}

object AssetCreation {

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  def validate(tx: AssetCreation): Try[Unit] = Try {
    require(tx.to.forall(_._2 >= 0L))
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
    require(tx.signatures.forall({ case (prop, signature) =>
      signature.isValid(prop, tx.messageToSign)
    }), "Invalid signatures")
  }

  def validatePrototype(tx: AssetCreation): Try[Unit] = Try {
    require(tx.to.forall(_._2 >= 0L))
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
  }

  /**
    * Route here from AssetApiRoute
    * Assumes that the WalletTrait contains the issuer's key information
    * Takes WalletTrait from current view, and generates signature from issuer's public key
    * Forms corresponding AssetCreation transaction
    */
  def createAndApply(w: Wallet,
                     to: IndexedSeq[(PublicKey25519Proposition, Long)],
                     fee: Long,
                     issuer: PublicKey25519Proposition,
                     assetCode: String,
                     data: String): Try[AssetCreation] = Try {

    val selectedSecret = w.secretByPublicImage(issuer).get
    val timestamp = Instant.now.toEpochMilli
    val messageToSign = AssetCreation(to, Map(), assetCode, issuer, fee, timestamp, data).messageToSign

    val signatures = Map(issuer -> PrivateKey25519Companion.sign(selectedSecret, messageToSign))

    AssetCreation(to, signatures, assetCode, issuer, fee, timestamp, data)
  }

  def createPrototype(to: IndexedSeq[(PublicKey25519Proposition, Long)],
                     fee: Long,
                     issuer: PublicKey25519Proposition,
                     assetCode: String,
                     data: String): Try[AssetCreation] = Try {

    val timestamp = Instant.now.toEpochMilli

    AssetCreation(to, Map(), assetCode, issuer, fee, timestamp, data)
  }

  implicit val decodeAssetCreation: Decoder[AssetCreation] = (c: HCursor) => for {
    rawTo <- c.downField("to").as[IndexedSeq[(String, String)]]
    rawSignatures <- c.downField("signatures").as[Map[String, String]]
    assetCode <- c.downField("assetCode").as[String]
    rawIssuer <- c.downField("issuer").as[String]
    fee <- c.downField("fee").as[Long]
    timestamp <- c.downField("timestamp").as[Long]
    data <- c.downField("data").as[String]
  } yield {
    val to = rawTo.map(t => Transaction.stringToPubKey(t._1) -> t._2.toLong)
    val signatures = rawSignatures.map { case (key, value) =>
        if(value == "") {
          (Transaction.stringToPubKey(key), Signature25519(Array.fill(Curve25519.SignatureLength)(1.toByte)))
        } else {
          (Transaction.stringToPubKey(key), Transaction.stringToSignature(value))
        }
    }
    val issuer = Transaction.stringToPubKey(rawIssuer)

    AssetCreation(
      to,
      signatures,
      assetCode,
      issuer,
      fee,
      timestamp,
      data
    )
  }
}