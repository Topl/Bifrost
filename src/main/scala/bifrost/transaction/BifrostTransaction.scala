package bifrost.transaction

import java.time.Instant

import bifrost.BifrostApp
import bifrost.contract.{Agreement, Contract}
import bifrost.exceptions.TransactionValidationException
import bifrost.scorexMod.{GenericBoxTransaction, GenericWalletBox}
import bifrost.transaction.BifrostTransaction.{Nonce, Value}
import bifrost.transaction.bifrostTransaction.ContractCompletion.assetNonce
import bifrost.transaction.Role.Role
import bifrost.transaction.box._
import bifrost.transaction.box.proposition.{MofNProposition, MofNPropositionSerializer}
import bifrost.transaction.proof.MultiSignature25519
import bifrost.wallet.BWallet
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.parser.parse
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, HCursor, Json}
import io.iohk.iodb.ByteArrayWrapper
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.settings.Settings
import bifrost.transaction.account.PublicKeyNoncedBox
import bifrost.transaction.bifrostTransaction._
import bifrost.transaction.box.BoxUnlocker
import bifrost.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.transaction.proof.{Proof, Signature25519}
import bifrost.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.io.Source
import scala.util.{Failure, Success, Try}

trait TransactionSettings extends Settings

trait BifrostTransaction
  extends GenericBoxTransaction[ProofOfKnowledgeProposition[PrivateKey25519], Any, BifrostBox] {
  lazy val bloomTopics: Option[IndexedSeq[Array[Byte]]] = None

  val boxIdsToOpen: IndexedSeq[Array[Byte]]

  implicit lazy val settings = new TransactionSettings {
    val testnetEndowment: Nonce = 20L
    override lazy val settingsJSON: Map[String, Json] = settingsFromFile(BifrostApp.settingsFilename)

    override def settingsFromFile(filename: String): Map[String, Json] = Try {
      val jsonString = Source.fromFile(filename).mkString
      parse(jsonString).right.get
    }
      .recoverWith {
        case _ =>
          Try {
            val jsonString = Source.fromURL(getClass.getResource(s"/$filename")).mkString
            parse(jsonString).right.get
          }
      }
      .toOption
      .flatMap(_.asObject)
      .map(_.toMap)
      .getOrElse(Map())
  }

}

object BifrostTransaction {
  type Nonce = Long
  type Value = Long

  def stringToPubKey(rawString: String): PublicKey25519Proposition =
    PublicKey25519Proposition(Base58.decode(rawString).get)

  def stringToSignature(rawString: String): Signature25519 = Signature25519(Base58.decode(rawString).get)

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))
}









object Role extends Enumeration {
  type Role = Value
  val Producer: Role = Value("producer")
  val Investor: Role = Value("investor")
  val Hub: Role = Value("hub")
}










/*abstract class TestTransaction extends BifrostTransaction

/**
  *
  *
  * @param totalAssetBoxes     Map of asset hub pair to an indexed sequence tuple of the asset box to convert and its nonce
  * @param assetsToReturn      Map of a tuple of the asset code and hub to a tuple of an asset box and amount returned
  * @param assetTokensToRedeem Map of asset code to a tuple of asset box and amount being redeemed
  * @param conversionSignatures
  * @param fee
  * @param timestamp
  * @param data
  */
case class ConversionTransaction(totalAssetBoxes: Map[(String, PublicKey25519Proposition), IndexedSeq[(PublicKey25519Proposition, Nonce)]],
                                 assetsToReturn: Map[(String, PublicKey25519Proposition), IndexedSeq[(PublicKey25519Proposition, Long)]],
                                 assetTokensToRedeem: Map[(String, PublicKey25519Proposition), IndexedSeq[(PublicKey25519Proposition, Long)]],
                                 conversionSignatures: Map[(String, PublicKey25519Proposition), IndexedSeq[Signature25519]],
                                 override val fee: Long,
                                 override val timestamp: Long,
                                 data: String)
  extends TestTransaction {

  import ConversionTransaction._

  override type M = ConversionTransaction

  /* make sure that boxes and signatures line up properly
     it is a map of an array of box ids as keys to matching signatures as values */
  val assetGroup: Map[ByteArrayWrapper, Signature25519] = totalAssetBoxes.flatMap(entry =>
    entry._2.map(t => ByteArrayWrapper(
      PublicKeyNoncedBox.idFromBox(t._1,
        t
          ._2)))
      .zip(conversionSignatures(entry
        ._1)))

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = assetGroup.keySet.toIndexedSeq.map(_.data).sortBy(Base58.encode)

  /* unlocks boxes by boxId and signature from assetGroup */
  override lazy val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] = boxIdsToOpen.map {
    boxId =>
      new BoxUnlocker[PublicKey25519Proposition] {
        override val closedBoxId: Array[Byte] = boxId
        override val boxKey: Signature25519 = assetGroup(ByteArrayWrapper(boxId))
      }
  }

  override lazy val serializer = ConversionTransactionCompanion

  override def toString: String = s"ConversionTransaction(${json.noSpaces})"

  override lazy val json: Json = Map(
    "txHash" -> Base58.encode(id).asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).asJson,
    "totalAssetBoxes" -> boxIdsToOpen.map(id => Base58.encode(id).asJson).asJson,
    "assetsToReturn" -> assetsToReturn.map { case (assetHub: (String, PublicKey25519Proposition),
    returned: (IndexedSeq[(PublicKey25519Proposition, Long)])) =>
      returned.map(prop =>
        Map(
          "assetCode" -> assetHub._1.asJson,
          "issuer" -> Base58.encode(assetHub._2.pubKeyBytes).asJson,
          "proposition" -> Base58.encode(prop._1.pubKeyBytes).asJson,
          "amount" -> prop._2.asJson
        ).asJson
      )
    }.asJson,
    "assetTokensToRedeem" -> assetTokensToRedeem.map { case (assetHub: (String, PublicKey25519Proposition),
    redeem: (IndexedSeq[(PublicKey25519Proposition, Long)])) =>
      redeem.map(prop =>
        Map(
          "assetCode" -> assetHub._1.asJson,
          "issuer" -> Base58.encode(assetHub._2.pubKeyBytes).asJson,
          "proposition" -> Base58.encode(prop._1.pubKeyBytes).asJson,
          "amount" -> prop._2.asJson
        ).asJson
      )
    }.asJson,
    "conversionSignatures" -> conversionSignatures.map { case (assetHub: (String, PublicKey25519Proposition),
    signatures: (IndexedSeq[(Signature25519)])) =>
      signatures.map(sig =>
        Map(
          "assetCode" -> assetHub._1.asJson,
          "issuer" -> Base58.encode(assetHub._2.pubKeyBytes).asJson,
          "signature" -> Base58.encode(sig.signature).asJson
        ).asJson
      )
    }.asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson,
    "data" -> data.asJson
  ).asJson

  /* Creates new AssetBoxes specified by the assetCode and Long from the returnedAssets parameter and
      hub from the hubs parameter, */
  override lazy val newBoxes: Traversable[BifrostBox] = {
    assetsToReturn.flatMap { case (assetHub, returned) =>
      returned.zipWithIndex.map { case (propAmount, idx) =>
        val nonce = BifrostTransaction.nonceFromDigest(
          FastCryptographicHash(Bytes.concat(
            "ConversionTransactionAssets".getBytes,
            hashNoNonces,
            propAmount._1.pubKeyBytes,
            Longs.toByteArray(propAmount._2),
            Ints.toByteArray(idx)))
        )
        AssetBox(propAmount._1, nonce, propAmount._2, assetHub._1, assetHub._2, data)
      }
    }

    assetTokensToRedeem.flatMap { case (assetHub, boxes) =>
      boxes.zipWithIndex.map { case ((prop, amount), idx) =>
        val nonce = BifrostTransaction.nonceFromDigest(
          FastCryptographicHash(Bytes.concat(
            "ConversionTransactionPolys".getBytes,
            hashNoNonces,
            prop.pubKeyBytes,
            Longs.toByteArray(amount),
            assetHub._1.getBytes,
            Ints.toByteArray(idx)))
        )
        /* Convert asset tokens to polyBox amount by asset code exchange rate */
        val polyAmount = conversionRates.get(assetHub._1)
        polyAmount match {
          //noinspection ScalaStyle
          case (None) => AssetBox(prop, nonce, amount, assetHub._1, assetHub._2, data)
          case (Some(rate)) =>
            val convertedAmount: Long = (amount.toDouble * rate).floor.toLong
            PolyBox(prop, nonce, convertedAmount)
        }
      }
    }
  }

  override lazy val messageToSign: Array[Byte] = {
    FastCryptographicHash(Bytes.concat(
      "ConversionTransaction".getBytes, hashNoNonces
    ))
  }

  lazy val hashNoNonces = FastCryptographicHash(
    assetsToReturn.values.foldLeft(Array[Byte]())((a, b) => a ++ b.flatMap(_._1.pubKeyBytes) ++
      assetsToReturn.keys.foldLeft(Array[Byte]())((a, b) => a ++ b._2.pubKeyBytes)) ++
      unlockers.map(_.closedBoxId).foldLeft(Array[Byte]())(_ ++ _) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)
  )
}

object ConversionTransaction {
  val conversionRates: Map[String, Double] = Map("Sheep" -> 5,
    "Wood" -> .895,
    "Brick" -> .5,
    "Ore" -> 1.2,
    "grain" -> 1)

  def validate(tx: ConversionTransaction): Try[Unit] = Try {

    //check that all of the signatures are valid for every box
    require(tx.conversionSignatures.forall {
      case (assetHub: (String, PublicKey25519Proposition), sigs: IndexedSeq[Signature25519]) =>
        val totalBoxes = tx.totalAssetBoxes(assetHub)
        sigs.length == totalBoxes.length &&
          sigs.zip(totalBoxes.map(_._1)).forall {
            case (sig: Signature25519, prop: PublicKey25519Proposition) => sig.isValid(prop, tx.messageToSign)
          }
    })

    require(tx.assetsToReturn.keySet.subsetOf(tx.totalAssetBoxes.keySet))
    require(tx.assetTokensToRedeem.keySet.subsetOf(tx.totalAssetBoxes.keySet))
    require(tx.conversionSignatures.keySet.subsetOf(tx.totalAssetBoxes.keySet))

    require((tx.assetsToReturn.keySet ++ tx.assetTokensToRedeem.keySet) equals tx.totalAssetBoxes.keySet)

    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
  }
}*/
