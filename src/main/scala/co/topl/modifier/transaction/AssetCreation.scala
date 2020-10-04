package co.topl.modifier.transaction

import java.time.Instant

import co.topl.crypto.{FastCryptographicHash, Signature25519}
import co.topl.modifier.transaction.Transaction.Nonce
import co.topl.modifier.transaction.serialization.AssetCreationSerializer
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.nodeView.state.box.{AssetBox, Box, TokenBox}
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success, Try}

case class AssetCreation (to                    : IndexedSeq[(PublicKey25519Proposition, Long)],
                          signatures            : Map[PublicKey25519Proposition, Signature25519],
                          assetCode             : String,
                          issuer                : PublicKey25519Proposition,
                          override val fee      : Long,
                          override val timestamp: Long,
                          data                  : String
                         ) extends Transaction {

  override type M = AssetCreation

  lazy val serializer: AssetCreationSerializer.type = AssetCreationSerializer

  override lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq()

  //TODO deprecate timestamp once fee boxes are included in nonce generation
  lazy val hashNoNonces: Array[Byte] = FastCryptographicHash(
    to.map(_._1.pubKeyBytes).reduce(_ ++ _) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)
  )

  override lazy val newBoxes: Traversable[TokenBox] = {
    to
      .filter(toInstance => toInstance._2 > 0L)
      .zipWithIndex
      .map { case ((prop, value), idx) =>
        val nonce = AssetCreation.nonceFromDigest(
          FastCryptographicHash(
            "AssetCreation".getBytes ++
              prop.pubKeyBytes ++
              issuer.pubKeyBytes ++
              assetCode.getBytes ++
              hashNoNonces ++
              Ints.toByteArray(idx)
          )
        )
        AssetBox(prop, nonce, value, assetCode, issuer, data)
      }
  }

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    "AssetCreation".getBytes(),
    to.map(_._1.pubKeyBytes).reduce(_ ++ _) ++
      newBoxes.foldLeft(Array[Byte]())((acc, x) => acc ++ x.bytes),
    issuer.pubKeyBytes,
    assetCode.getBytes,
    Longs.toByteArray(fee),
    data.getBytes
  )

  override def toString: String = s"AssetCreation(${json.noSpaces})"

  override def json: Json = AssetCreation.jsonEncoder(this)

}

object AssetCreation {

  type SR = StateReader[Box]

  def nonceFromDigest (digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  /**
    * Route here from AssetApiRoute
    * Assumes that the WalletTrait contains the issuer's key information
    * Takes WalletTrait from current view, and generates signature from issuer's public key
    * Forms corresponding AssetCreation transaction
    */
//  def createAndApply (w        : Wallet,
//                      to       : IndexedSeq[(PublicKey25519Proposition, Long)],
//                      fee      : Long,
//                      issuer   : PublicKey25519Proposition,
//                      assetCode: String,
//                      data     : String
//                     ): Try[AssetCreation] = Try {
//
//    val selectedSecret = w.secretByPublicImage(issuer).get
//    val timestamp = Instant.now.toEpochMilli
//    val messageToSign = AssetCreation(to, Map(), assetCode, issuer, fee, timestamp, data).messageToSign
//
//    val signatures = Map(issuer -> selectedSecret.sign(messageToSign))
//
//    AssetCreation(to, signatures, assetCode, issuer, fee, timestamp, data)
//  }

  def createPrototype (to       : IndexedSeq[(PublicKey25519Proposition, Long)],
                       fee      : Long,
                       issuer   : PublicKey25519Proposition,
                       assetCode: String,
                       data     : String
                      ): Try[AssetCreation] = Try {

    val timestamp = Instant.now.toEpochMilli

    AssetCreation(to, Map(), assetCode, issuer, fee, timestamp, data)
  }

  def syntacticValidate (tx: AssetCreation, withSigs: Boolean = true): Try[Unit] = Try {
    require(tx.to.forall(_._2 > 0L))
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)

    if (withSigs) {
      require(tx.signatures.forall({ case (prop, signature) =>
        signature.isValid(prop, tx.messageToSign)
      }),
        "Invalid signatures"
      )
    }

    tx.newBoxes.size match {
      //only one box should be created
      case 1 if ( tx.newBoxes.head.isInstanceOf[AssetBox] ) => Success(Unit)
      case _                                                => Failure(new Exception("Invalid transaction"))
    }
  }

  def validatePrototype (tx: AssetCreation): Try[Unit] = syntacticValidate(tx, withSigs = false)

  def semanticValidate (tx: AssetCreation, state: SR): Try[Unit] = {

    // check that the transaction is correctly formed before checking state
    syntacticValidate(tx)

  }

  implicit val jsonEncoder: Encoder[AssetCreation] = { tx: AssetCreation
    Map(
      "txHash" -> tx.id.toString.asJson,
      "txType" -> "AssetCreation".asJson,
      "newBoxes" -> tx.newBoxes.map(b => b.id.toString).toSeq.asJson,
      "to" -> tx.to.map { case (prop, value) => prop.toString -> value.toString}.asJson,
      "issuer" -> tx.issuer.toString.asJson,
      "assetCode" -> tx.assetCode.asJson,
      "signatures" -> tx.signatures.map { case (prop, sig) => prop.toString -> sig.signature.toString}.asJson,
      "fee" -> tx.fee.asJson,
      "timestamp" -> tx.timestamp.asJson,
      "data" -> tx.data.asJson
    ).asJson
  }

  implicit val jsonDecoder: Decoder[AssetCreation] = (c: HCursor) =>
    for {
      rawTo <- c.downField("to").as[IndexedSeq[(String, String)]]
      rawSignatures <- c.downField("signatures").as[Map[String, String]]
      assetCode <- c.downField("assetCode").as[String]
      rawIssuer <- c.downField("issuer").as[String]
      fee <- c.downField("fee").as[Long]
      timestamp <- c.downField("timestamp").as[Long]
      data <- c.downField("data").as[String]
    } yield {
      val to = rawTo.map(t => PublicKey25519Proposition(t._1) -> t._2.toLong)
      val issuer = PublicKey25519Proposition(rawIssuer)
      val signatures = rawSignatures.map { case (key, value) =>
        if (value.isEmpty) (PublicKey25519Proposition(key), Signature25519(Array.empty[Byte]))
        else (PublicKey25519Proposition(key), Signature25519(value))
      }

      AssetCreation(to, signatures, assetCode, issuer, fee, timestamp, data)
    }
}
