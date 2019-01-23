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
import bifrost.transaction.bifrostTransaction.ContractTransaction
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

case class CoinbaseTransaction (to: IndexedSeq[(PublicKey25519Proposition, Long)],
                                signatures: IndexedSeq[Signature25519],
                                override val timestamp: Long,
                                blockID: Array[Byte]) extends BifrostTransaction {
  override type M = CoinbaseTransaction

  lazy val serializer = CoinbaseTransactionCompanion

  override def toString: String = s"CoinbaseTransaction(${json.noSpaces})"

  lazy val fee = 0L // you don't ever pay for a Coinbase TX since you'd be paying yourself so fee must equal 0

  override lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq()

  override lazy val unlockers: Traversable[BoxUnlocker[ProofOfKnowledgeProposition[PrivateKey25519]]] = Traversable()

  lazy val hashNoNonces = FastCryptographicHash(
    to.head._1.pubKeyBytes ++ Longs.toByteArray(timestamp) ++ Longs.toByteArray(fee) ++ blockID // message that gets hashed
  )

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  val nonce = nonceFromDigest(FastCryptographicHash(
    "CoinbaseTransaction".getBytes ++ hashNoNonces
  ))

  lazy val newBoxes: Traversable[BifrostBox] = Traversable(ArbitBox(to.head._1, nonce, to.head._2))

  override lazy val json: Json = Map( // tx in json form
    "id" -> Base58.encode(id).asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).asJson,
    "to" -> Map(
        "proposition" -> Base58.encode(to.head._1.pubKeyBytes).asJson,
        "value" -> to.head._2.asJson
      ).asJson,
    "blockID" -> Base58.encode(blockID).asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  def commonMessageToSign: Array[Byte] = newBoxes.head.bytes ++ // is the new box + the timestamp + the fee,
    Longs.toByteArray(timestamp) ++
    Longs.toByteArray(fee) ++
    blockID

  override lazy val messageToSign: Array[Byte] = Bytes.concat( // just tac on the byte string "CoinbaseTransaction" to the beginning of the common message
    "CoinbaseTransaction".getBytes(),
    commonMessageToSign
  )
}

object CoinbaseTransaction {

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES)) // take in a byte array and return a nonce (long)

  def validate(tx: CoinbaseTransaction): Try[Unit] = Try {
    require(tx.to.head._2 >= 0L) // can't make negative Arbits. anti-Arbits?!?!
    require(tx.fee == 0)
    require(tx.timestamp >= 0)
    require(tx.signatures.forall({ signature => // should be only one sig
      signature.isValid(tx.to.head._1, tx.messageToSign) // because this is set to self the signer is also the reciever
    }), "Invalid signature")
  }

  def createAndApply(w: BWallet,
                     to: IndexedSeq[(PublicKey25519Proposition, Long)],
                     blockID: Array[Byte]  // the blockID of the parent block. EX: if this is the CB for block 100 the blockID would be the id of block 99
                    ): Try[CoinbaseTransaction] = Try {
    val selectedSecret = w.secretByPublicImage(to.head._1).get // use the receiver's pub-key to generate secret
    val fakeSigs = IndexedSeq(Signature25519(Array())) // create an index sequence of empty sigs
    val timestamp = Instant.now.toEpochMilli // generate timestamp
    val messageToSign = CoinbaseTransaction(to, fakeSigs, timestamp, blockID).messageToSign // using your fake sigs generate a CB tx and get its msg to sign
    val signatures = IndexedSeq(PrivateKey25519Companion.sign(selectedSecret, messageToSign)) // sign the msg you just generated
    CoinbaseTransaction(to, signatures, timestamp, blockID) // use the sigs you just generated to make the real CB tx
  }


}

case class AssetCreation (to: IndexedSeq[(PublicKey25519Proposition, Long)],
                          signatures: IndexedSeq[Signature25519],
                          assetCode: String,
                          val issuer: PublicKey25519Proposition,
                          override val fee: Long,
                          override val timestamp: Long,
                          val data: String) extends BifrostTransaction {


  override type M = AssetCreation

  lazy val serializer = AssetCreationCompanion

  override def toString: String = s"AssetCreation(${json.noSpaces})"

  override lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq()

  override lazy val unlockers: Traversable[BoxUnlocker[ProofOfKnowledgeProposition[PrivateKey25519]]] = Traversable()

  lazy val hashNoNonces = FastCryptographicHash(
  to.map(_._1.pubKeyBytes).reduce(_ ++ _) ++
    Longs.toByteArray(timestamp) ++
    Longs.toByteArray(fee)
  )

  override lazy val newBoxes: Traversable[BifrostBox] = to.zipWithIndex.map {
   case ((prop, value), idx) =>
     val nonce = AssetCreation.nonceFromDigest(FastCryptographicHash(
       "AssetCreation".getBytes ++
         prop.pubKeyBytes ++
         issuer.pubKeyBytes ++
         assetCode.getBytes ++
         hashNoNonces ++
         Ints.toByteArray(idx)
     ))

     //TODO assetBoxes elsewhere do not subtract fee from box value
     //TODO no check that amount >= fee
     //AssetBox(prop, nonce, value, assetCode, hub)
     AssetBox(prop, nonce, value - fee, assetCode, issuer, data)
   }

  override lazy val json: Json = Map(
    "txHash" -> Base58.encode(id).asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).asJson,
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

  def commonMessageToSign: Array[Byte] = (if (newBoxes.nonEmpty) {
  newBoxes
    .map(_.bytes)
    .reduce(_ ++ _)
  } else {
    Array[Byte]()
  }) ++
    Longs.toByteArray(timestamp) ++
    Longs.toByteArray(fee)

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
  "AssetCreation".getBytes(),
  commonMessageToSign,
  issuer.pubKeyBytes,
  assetCode.getBytes,
  data.getBytes
  )

}

object AssetCreation {

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  def validate(tx: AssetCreation): Try[Unit] = Try {
    //require(tx.from.size == tx.signatures.size)
    require(tx.to.forall(_._2 >= 0L))
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
    require(tx.signatures.forall({ case (signature) =>
      //println(signature.isValid(tx.hub, tx.messageToSign))
      signature.isValid(tx.issuer, tx.messageToSign)
    }), "Invalid signatures")
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
    val messageToSign = AssetCreation(to, fakeSigs, assetCode, issuer, fee, timestamp, data).messageToSign

    val signatures = IndexedSeq(PrivateKey25519Companion.sign(selectedSecret, messageToSign))

    AssetCreation(to, signatures, assetCode, issuer, fee, timestamp, data)
  }
}

object Role extends Enumeration {
  type Role = Value
  val Producer: Role = Value("producer")
  val Investor: Role = Value("investor")
  val Hub: Role = Value("hub")
}





abstract class TransferTransaction(val from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                                   val to: IndexedSeq[(PublicKey25519Proposition, Long)],
                                   val signatures: IndexedSeq[Signature25519],
                                   override val fee: Long,
                                   override val timestamp: Long) extends BifrostTransaction {

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = from.map { case (prop, nonce) =>
    PublicKeyNoncedBox.idFromBox(prop, nonce)
  }

  override lazy val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] = boxIdsToOpen.zip(signatures).map {
    case (boxId, signature) =>
      new BoxUnlocker[PublicKey25519Proposition] {
        override val closedBoxId: Array[Byte] = boxId
        override val boxKey: Signature25519 = signature
      }
  }

  lazy val hashNoNonces = FastCryptographicHash(
    to.map(_._1.pubKeyBytes).reduce(_ ++ _) ++
      unlockers.map(_.closedBoxId).reduce(_ ++ _) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)
  )

  override lazy val json: Json = Map(
    "txHash" -> Base58.encode(id).asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).asJson,
    "boxesToRemove" -> boxIdsToOpen.map(id => Base58.encode(id).asJson).asJson,
    "from" -> from.map { s =>
      Map(
        "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
        "nonce" -> s._2.toString.asJson
      ).asJson
    }.asJson,
    "to" -> to.map { s =>
      Map(
        "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
        "value" -> s._2.toString.asJson
      ).asJson
    }.asJson,
    "signatures" -> signatures
      .map(s => Base58.encode(s.signature).asJson)
      .asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  def commonMessageToSign: Array[Byte] = (if (newBoxes.nonEmpty) {
    newBoxes
      .map(_.bytes)
      .reduce(_ ++ _)
  } else {
    Array[Byte]()
  }) ++
    unlockers.map(_.closedBoxId).reduce(_ ++ _) ++
    Longs.toByteArray(timestamp) ++
    Longs.toByteArray(fee)
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

case class PolyTransfer(override val from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                        override val to: IndexedSeq[(PublicKey25519Proposition, Long)],
                        override val signatures: IndexedSeq[Signature25519],
                        override val fee: Long,
                        override val timestamp: Long,
                        val data: String)
  extends TransferTransaction(from, to, signatures, fee, timestamp) {

  override type M = PolyTransfer

  override lazy val serializer = PolyTransferCompanion

  override def toString: String = s"PolyTransfer(${json.noSpaces})"

  override lazy val newBoxes: Traversable[BifrostBox] = to.zipWithIndex.map {
    case ((prop, value), idx) =>
      val nonce = PolyTransfer
        .nonceFromDigest(FastCryptographicHash("PolyTransfer".getBytes
          ++ prop.pubKeyBytes
          ++ hashNoNonces
          ++ Ints.toByteArray(idx)))

      PolyBox(prop, nonce, value)
  }

  override lazy val messageToSign: Array[Byte] = "PolyTransfer".getBytes() ++ super.commonMessageToSign ++ data.getBytes
}


object PolyTransfer extends TransferUtil {

  def apply(from: IndexedSeq[(PrivateKey25519, Nonce)],
            to: IndexedSeq[(PublicKey25519Proposition, Value)],
            fee: Long,
            timestamp: Long,
            data: String): PolyTransfer = {
    val params = parametersForApply(from, to, fee, timestamp, "PolyTransfer", data).get
    PolyTransfer(params._1, to, params._2, fee, timestamp, data)
  }

  def create(w: BWallet, toReceive: IndexedSeq[(PublicKey25519Proposition, Long)], fee: Long, data: String, publicKeyToSendFrom: Vector[String] = Vector(), publicKeyToSendChangeTo: String = "") = Try {
    val params = parametersForCreate(w, toReceive, fee, "PolyTransfer", publicKeyToSendFrom, publicKeyToSendChangeTo)
    val timestamp = Instant.now.toEpochMilli
    PolyTransfer(params._1.map(t => t._1 -> t._2), params._2, fee, timestamp, data)
  }

//  def createByKey(w: BWallet, toReceive: IndexedSeq[(PublicKey25519Proposition, Long)], fee: Long, data: String, publicKeyToSendFrom: Seq[Json]) = Try {
//        println()
//        println("Entered createByKey")
//        val params = parametersForCreate(w, toReceive, fee, "PolyTransfer", "")
//        val timestamp = Instant.now.toEpochMilli
//        PolyTransfer(params._1.map(t => t._1 -> t._2), params._2, fee, timestamp, data)
//      }//

  def validate(tx: PolyTransfer): Try[Unit] = validateTx(tx)
}

case class ArbitTransfer(override val from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                         override val to: IndexedSeq[(PublicKey25519Proposition, Long)],
                         override val signatures: IndexedSeq[Signature25519],
                         override val fee: Long,
                         override val timestamp: Long,
                         val data: String)
  extends TransferTransaction(from, to, signatures, fee, timestamp) {

  override type M = ArbitTransfer

  override lazy val serializer = ArbitTransferCompanion

  override def toString: String = s"ArbitTransfer(${json.noSpaces})"

  override lazy val newBoxes: Traversable[BifrostBox] = to.zipWithIndex.map {
    case ((prop, value), idx) =>
      val nonce = ArbitTransfer
        .nonceFromDigest(FastCryptographicHash("ArbitTransfer".getBytes
          ++ prop.pubKeyBytes
          ++ hashNoNonces
          ++ Ints.toByteArray(idx)))

      ArbitBox(prop, nonce, value)
  }

  override lazy val messageToSign: Array[Byte] = "ArbitTransfer".getBytes() ++ super.commonMessageToSign ++ data.getBytes
}

//noinspection ScalaStyle
object ArbitTransfer extends TransferUtil {

  def apply(from: IndexedSeq[(PrivateKey25519, Nonce)],
            to: IndexedSeq[(PublicKey25519Proposition, Value)],
            fee: Long,
            timestamp: Long,
            data: String): ArbitTransfer = {
    val params = parametersForApply(from, to, fee, timestamp, "ArbitTransfer", data).get
    ArbitTransfer(params._1, to, params._2, fee, timestamp, data)
  }

  def create(w: BWallet, toRecieve: IndexedSeq[(PublicKey25519Proposition, Long)], fee: Long, data: String, publicKeyToSendFrom: Vector[String] = Vector(), publicKeyToSendChangeTo: String = ""): Try[ArbitTransfer] = Try
  {

    val params = parametersForCreate(w, toRecieve, fee, "ArbitTransfer", publicKeyToSendFrom, publicKeyToSendChangeTo)
    val timestamp = Instant.now.toEpochMilli
    ArbitTransfer(params._1.map(t => t._1 -> t._2), params._2, fee, timestamp, data)
  }

  def validate(tx: ArbitTransfer): Try[Unit] = validateTx(tx)
}

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

  override lazy val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] = boxIdsToOpen
    .map {
      boxId =>
        new BoxUnlocker[PublicKey25519Proposition] {
          override val closedBoxId: Array[Byte] = boxId
          override val boxKey: Signature25519 = redemptionGroup(ByteArrayWrapper(boxId))
        }
    }

  lazy val hashNoNonces = FastCryptographicHash(
    remainderAllocations.values.foldLeft(Array[Byte]())((a, b) => a ++ b.flatMap(_._1.pubKeyBytes)) ++
      unlockers.map(_.closedBoxId).foldLeft(Array[Byte]())(_ ++ _) ++
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
