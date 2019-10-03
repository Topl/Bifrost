package bifrost.transaction.bifrostTransaction

import java.time.Instant

import bifrost.crypto.hash.FastCryptographicHash
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.box.{AssetBox, BifrostBox}
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.state.PrivateKey25519Companion
import bifrost.transaction.bifrostTransaction.BifrostTransaction.Nonce
import bifrost.transaction.serialization.AssetCreationCompanion
import bifrost.wallet.BWallet
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.util.Try

case class AssetCreation (to: IndexedSeq[(PublicKey25519Proposition, Long)],
                          signatures: Map[PublicKey25519Proposition, Signature25519],
                          assetCode: String,
                          issuer: PublicKey25519Proposition,
                          override val fee: Long,
                          override val timestamp: Long,
                          data: String) extends BifrostTransaction {


  override type M = AssetCreation

  lazy val serializer = AssetCreationCompanion

  override def toString: String = s"AssetCreation(${json.noSpaces})"

  override lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq()

  //TODO deprecate timestamp once fee boxes are included in nonce generation
  lazy val hashNoNonces = FastCryptographicHash(
    to.map(_._1.pubKeyBytes).reduce(_ ++ _) ++
    Longs.toByteArray(timestamp) ++
    Longs.toByteArray(fee)
  )

   override lazy val newBoxes: Traversable[BifrostBox] = to
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
    "txHash" -> Base58.encode(id).asJson,
    "txType" -> "AssetCreation".asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).asJson,
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
      }.asJson,    "fee" -> fee.asJson,
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
    * Assumes that the Wallet contains the issuer's key information
    * Takes Wallet from current view, and generates signature from issuer's public key
    * Forms corresponding AssetCreation transaction
    */
  def createAndApply(w: BWallet,
                     to: IndexedSeq[(PublicKey25519Proposition, Long)],
                     fee: Long,
                     issuer: PublicKey25519Proposition,
                     assetCode: String,
                     data: String): Try[AssetCreation] = Try {

    val selectedSecret = w.secretByPublicImage(issuer).get
    val fakeSigs = IndexedSeq(Signature25519(Array()))
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
}