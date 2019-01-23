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







trait TransferUtil {

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  def parametersForApply(from: IndexedSeq[(PrivateKey25519, Nonce)],
                         to: IndexedSeq[(PublicKey25519Proposition, Value)],
                         fee: Long,
                         timestamp: Long,
                         txType: String,
                         extraArgs: Any*):
  Try[(IndexedSeq[(PublicKey25519Proposition, Nonce)], IndexedSeq[Signature25519])] = Try {
    val fromPub = from.map { case (pr, n) => pr.publicImage -> n }
    val fakeSigs = from.map(_ => Signature25519(Array()))

    val undersigned = txType match {
      case "PolyTransfer" => PolyTransfer(fromPub, to, fakeSigs, fee, timestamp, extraArgs(0).asInstanceOf[String])
      case "ArbitTransfer" => ArbitTransfer(fromPub, to, fakeSigs, fee, timestamp, extraArgs(0).asInstanceOf[String])
      case "AssetTransfer" => AssetTransfer(
        fromPub,
        to,
        fakeSigs,
        extraArgs(0).asInstanceOf[PublicKey25519Proposition],
        extraArgs(1).asInstanceOf[String],
        fee,
        timestamp,
        extraArgs(2).asInstanceOf[String]
      )
    }

    val msg = undersigned.messageToSign
    val sigs = from.map { case (priv, _) => PrivateKey25519Companion.sign(priv, msg) }
    (fromPub, sigs)
  }

  //noinspection ScalaStyle
  def parametersForCreate(w: BWallet,
                          toReceive: IndexedSeq[(PublicKey25519Proposition, Long)],
                          fee: Long,
                          txType: String,
                          publicKeysToSendFrom: Vector[String],
                          publicKeyToSendChangeTo: String,
                          extraArgs: Any*):
  (IndexedSeq[(PrivateKey25519, Long, Long)], IndexedSeq[(PublicKey25519Proposition, Long)]) = {

    toReceive
      .foldLeft((IndexedSeq[(PrivateKey25519, Long, Long)](), IndexedSeq[(PublicKey25519Proposition, Long)]())) {
        case (a, (recipient, amount)) =>

          // Restrict box search to specified public keys if provided
          val keyFilteredBoxes: Seq[GenericWalletBox[Any, w.PI, BifrostBox]] =
            if (publicKeysToSendFrom.isEmpty) w.boxes() else publicKeysToSendFrom.flatMap(p => w.boxesByKey(p))

          // Match only the type of boxes specified by txType
          val keyAndTypeFilteredBoxes: Seq[BifrostPublic25519NoncedBox] = txType match {
            case "PolyTransfer" =>
              keyFilteredBoxes.flatMap(_.box match {
                case p: PolyBox => Some(p)
                case _ => None
              })
            case "ArbitTransfer" =>
              keyFilteredBoxes.flatMap(_.box match {
                case a: ArbitBox => Some(a)
                case _ => None
              })
            case "AssetTransfer" =>
              keyFilteredBoxes.flatMap(_.box match {
                case a: AssetBox
                  if (a.assetCode equals extraArgs(1).asInstanceOf[String]) &&
                    (a.issuer equals extraArgs(0)
                      .asInstanceOf[PublicKey25519Proposition]) =>
                  Some(a)
                case _ => None
              })
          }

          // Check if the keys currently unlocked in wallet match the proposition of any of the found boxes
          val senderInputBoxes: IndexedSeq[(PrivateKey25519, Long, Long)] = keyAndTypeFilteredBoxes
            .flatMap {
              b: BifrostPublic25519NoncedBox =>
                w.secretByPublicImage(b.proposition)
                  .map((_, b.nonce, b.value))
            }
            .toIndexedSeq

          // amount available to send in tx
          val canSend = senderInputBoxes.map(_._3).sum

          // Updated sender balance for specified box type (this is the change calculation for sender)
          val senderUpdatedBalance: (PublicKey25519Proposition, Long) = keyAndTypeFilteredBoxes.head match {
            case b: BifrostPublic25519NoncedBox =>
              publicKeyToSendChangeTo match {
                case "" => (b.proposition, canSend - amount - fee)
                case _ => (PublicKey25519Proposition(Base58.decode(publicKeyToSendChangeTo).get), canSend - amount - fee)
              }
            case _ => null
          }

          // create the list of outputs (senderChangeOut & recipientOut)
          val to: IndexedSeq[(PublicKey25519Proposition, Long)] = IndexedSeq(senderUpdatedBalance, (recipient, amount))

          require(senderInputBoxes.map(_._3).sum - to.map(_._2).sum == fee)
          (a._1 ++ senderInputBoxes, a._2 ++ to)
      }
  }

  def validateTx(tx: TransferTransaction): Try[Unit] = Try {
    require(tx.from.size == tx.signatures.size)
    require(tx.to.forall(_._2 >= 0L))
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
    require(tx.from.zip(tx.signatures).forall { case ((prop, _), proof) =>
      proof.isValid(prop, tx.messageToSign)
    })

  }
}














case class ProfileTransaction(from: PublicKey25519Proposition,
                              signature: Signature25519,
                              keyValues: Map[String, String],
                              override val fee: Long,
                              override val timestamp: Long)
  extends BifrostTransaction {

  override type M = ProfileTransaction

  override lazy val serializer = ProfileTransactionCompanion

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq[Array[Byte]]()

  override lazy val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] = boxIdsToOpen.map {
    boxId =>
      new BoxUnlocker[PublicKey25519Proposition] {
        override val closedBoxId: Array[Byte] = boxId
        override val boxKey: Signature25519 = signature
      }
  }

  lazy val hashNoNonces = FastCryptographicHash(
    from.pubKeyBytes ++
      keyValues.foldLeft(Array[Byte]())((a, b) => a ++ b._1.getBytes ++ b._2.getBytes) ++
      unlockers.map(_.closedBoxId).foldLeft(Array[Byte]())(_ ++ _) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)
  )

  override lazy val newBoxes: Traversable[BifrostBox] = keyValues.flatMap {
    case (key, value) =>
      if (value.equals("investor") && key.equals("role") && settings.isTestnet) {
        val digest = FastCryptographicHash("ProfileTransaction".getBytes ++ from.pubKeyBytes ++ hashNoNonces)
        val nonce = Longs.fromByteArray(digest.take(Longs.BYTES))
        Seq(ProfileBox(from, 0L, value, key)) :+ PolyBox(from, nonce, settings.testnetEndowment)
      } else {
        Seq(ProfileBox(from, 0L, value, key))
      }
  }

  override lazy val messageToSign: Array[Byte] = ProfileTransaction.messageToSign(timestamp, from, keyValues)

  override lazy val json: Json = Map(
    "txHash" -> Base58.encode(id).asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).asJson,
    "boxesToRemove" -> boxIdsToOpen.map(id => Base58.encode(id).asJson).asJson,
    "from" -> Base58.encode(from.pubKeyBytes).asJson,
    "signature" -> Base58.encode(signature.signature).asJson,
    "keyValues" -> keyValues.asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson
}

object ProfileTransaction {

  def messageToSign(timestamp: Long,
                    from: PublicKey25519Proposition,
                    keyValues: Map[String, String]): Array[Byte] = Bytes.concat(
    Longs.toByteArray(timestamp),
    from.pubKeyBytes,
    keyValues.asJson.toString().getBytes()
  )

  def validate(tx: ProfileTransaction): Try[Unit] = Try {
    // ensure no duplicates
    val keysSet = tx.keyValues.keys.toSet

    require(keysSet.subsetOf(ProfileBox.acceptableKeys))
    require(ProfileBox.acceptableRoleValues.contains(tx.keyValues("role")))
    require(tx.signature.isValid(tx.from, tx.messageToSign))
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
  }
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
