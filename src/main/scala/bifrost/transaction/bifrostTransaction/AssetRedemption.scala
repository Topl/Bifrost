package bifrost.transaction.bifrostTransaction

import bifrost.crypto.hash.FastCryptographicHash
import bifrost.transaction.bifrostTransaction.BifrostTransaction.Nonce
import bifrost.transaction.account.PublicKeyNoncedBox
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.box.{AssetBox, BifrostBox}
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.serialization.AssetRedemptionCompanion
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.{Decoder, HCursor, Json}
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.util.Try

case class AssetRedemption(availableToRedeem: Map[String, IndexedSeq[(PublicKey25519Proposition, Nonce)]],
                           remainderAllocations: Map[String, IndexedSeq[(PublicKey25519Proposition, Long)]],
                           signatures: Map[String, IndexedSeq[Signature25519]],
                           issuer: PublicKey25519Proposition,
                           fee: Long,
                           timestamp: Long,
                           data: String) extends BifrostTransaction {

  override type M = AssetRedemption

  override lazy val bloomTopics: Option[IndexedSeq[Array[Byte]]] = {
    val remainderKeys = remainderAllocations.flatMap {
      case (_, value) =>
        value.map(t => t._1.pubKeyBytes)
    }
    Option(IndexedSeq("AssetRedemption".getBytes ++ issuer.pubKeyBytes) ++ remainderKeys.toSet.take(3).toSeq)
  }

  val redemptionGroup: Map[ByteArrayWrapper, Signature25519] = availableToRedeem.flatMap(entry =>
    entry._2
      .map(t => ByteArrayWrapper(
        PublicKeyNoncedBox
          .idFromBox(t._1,
            t._2))).zip(
      signatures(entry._1))
  )

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = redemptionGroup
    .keys
    .toIndexedSeq
    .map(_.data)
    .sortBy(Base58.encode)

  lazy val hashNoNonces = FastCryptographicHash(
    remainderAllocations.values.foldLeft(Array[Byte]())((a, b) => a ++ b.flatMap(_._1.pubKeyBytes)) ++
      issuer.pubKeyBytes ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)
  )

  override val newBoxes: Traversable[BifrostBox] = remainderAllocations.flatMap { case (assetCode, remainder) =>
    remainder.zipWithIndex.map { case (r, i) =>

      val nonce = AssetRedemption.nonceFromDigest(
        FastCryptographicHash(Bytes.concat(
          "AssetRedemption".getBytes,
          hashNoNonces,
          r._1.pubKeyBytes,
          Longs.toByteArray(r._2),
          Ints.toByteArray(i)
        ))
      )
      AssetBox(r._1, nonce, r._2, assetCode, issuer, data)
    }
  }

  override lazy val serializer = AssetRedemptionCompanion

  override lazy val messageToSign: Array[Byte] = {
    FastCryptographicHash(Bytes.concat(
      "AssetRedemption".getBytes, hashNoNonces, data.getBytes
    ))
  }

  override lazy val json: Json = Map(
    "txHash" -> Base58.encode(id).asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).asJson,
    "boxesToRemove" -> boxIdsToOpen.map(id => Base58.encode(id).asJson).asJson,
    "availableToRedeem" -> availableToRedeem
      .map { case (assetCode: String, preBoxes: IndexedSeq[(PublicKey25519Proposition, Nonce)]) =>
        assetCode -> preBoxes.map(pb =>
          Map(
            "proposition" -> Base58.encode(pb._1.pubKeyBytes).asJson,
            "nonce" -> pb._2.toString.asJson
          ).asJson
        )
      }.asJson,
    "remainderAllocations" -> remainderAllocations
      .map { case (assetCode: String, afterBoxes: IndexedSeq[(PublicKey25519Proposition, Nonce)]) =>
        assetCode -> afterBoxes.map(ab =>
          Map(
            "proposition" -> Base58.encode(ab._1.pubKeyBytes).asJson,
            "nonce" -> ab._2.toString.asJson
          ).asJson
        )
      }.asJson,
    "signatures" -> signatures.map { case (assetCode: String, signatures: IndexedSeq[Signature25519]) =>
      assetCode -> signatures.map(s => Base58.encode(s.signature).asJson).asJson
    }.asJson,
    "issuer" -> Base58.encode(issuer.pubKeyBytes).asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson
}

object AssetRedemption {

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  def validate(tx: AssetRedemption): Try[Unit] = Try {

    // Check that all of the signatures are valid for all of the boxes
    require(tx.signatures.forall {
      case (assetCode: String, sigs: IndexedSeq[Signature25519]) =>
        val boxesToRedeem = tx.availableToRedeem(assetCode)
        sigs.length == boxesToRedeem.length &&
          sigs.zip(boxesToRedeem.map(_._1)).forall {
            case (sig: Signature25519, prop: PublicKey25519Proposition) => sig.isValid(prop, tx.messageToSign)
          }
    })

    // Check that all of the assets to be redeemed are consistent with assets provided
    require(tx.remainderAllocations.keySet.subsetOf(tx.availableToRedeem.keySet))

    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
  }

  implicit val decodeAssetRedemption: Decoder[AssetRedemption] = (c: HCursor) => for {
    availableToRedeemRaw <- c.downField("availableToRedeem").as[Map[String, IndexedSeq[(String, Long)]]]
    remainderAllocationsRaw <- c.downField("remainderAllocations").as[Map[String, IndexedSeq[(String, Long)]]]
    signaturesRaw <- c.downField("signatures").as[Map[String, IndexedSeq[String]]]
    issuerRaw <- c.downField("issuer").as[String]
    fee <- c.downField("fee").as[Long]
    timestamp <- c.downField("timestamp").as[Long]
    data <- c.downField("data").as[String]
  } yield {
    def convertToProp(value: IndexedSeq[(String, Long)]) = value.map {
      case (pubKeyString, nonce) =>
        (BifrostTransaction.stringToPubKey(pubKeyString), nonce)
    }

    val availableToRedeem = availableToRedeemRaw.map { case (key, value) => (key, convertToProp(value)) }
    val remainderAllocations = remainderAllocationsRaw.map { case (key, value) => (key, convertToProp(value)) }
    val signatures = signaturesRaw.map { case (key, values) =>
      val newValues = values.map(value =>
        if (value == "") {
          Signature25519(Array.fill(Curve25519.SignatureLength)(1.toByte))
        } else {

          BifrostTransaction.stringToSignature(value)
        }
      )
      (key, newValues)
    }
    val issuer = PublicKey25519Proposition(Base58.decode(issuerRaw).get)
    AssetRedemption(availableToRedeem, remainderAllocations, signatures, issuer, fee, timestamp, data)
  }
}