package bifrost.transaction

import java.time.Instant

import bifrost.BifrostApp
import bifrost.contract.{Agreement, Contract}
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.exceptions.TransactionValidationException
import bifrost.scorexMod.{GenericBoxTransaction, GenericWalletBox}
import bifrost.settings.Settings
import bifrost.transaction.BifrostTransaction.{Nonce, Value}
import bifrost.transaction.ContractCompletion.assetNonce
import bifrost.transaction.Role.Role
import bifrost.transaction.account.PublicKeyNoncedBox
import bifrost.transaction.box.proposition.{MofNProposition, MofNPropositionSerializer, ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.transaction.box.{BoxUnlocker, _}
import bifrost.transaction.proof.{MultiSignature25519, Proof, Signature25519}
import bifrost.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import bifrost.wallet.BWallet
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.parser.parse
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, HCursor, Json}
import io.iohk.iodb.ByteArrayWrapper
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519
import serializer.BuySellOrder

import scala.io.Source
import scala.util.{Failure, Success, Try}

trait TransactionSettings extends Settings

sealed trait BifrostTransaction
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


case class AssetCreation(val to: IndexedSeq[(PublicKey25519Proposition, Long)],
                         val signatures: IndexedSeq[Signature25519],
                         val assetCode: String,
                         val issuer: PublicKey25519Proposition,
                         override val fee: Long,
                         override val timestamp: Long,
                         val data: String) extends BifrostTransaction {


  override type M = AssetCreation

  lazy val serializer = AssetCreationCompanion
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
  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    "AssetCreation".getBytes(),
    commonMessageToSign,
    issuer.pubKeyBytes,
    assetCode.getBytes,
    data.getBytes
  )

  override def toString: String = s"AssetCreation(${json.noSpaces})"

  def commonMessageToSign: Array[Byte] = (if (newBoxes.nonEmpty) {
    newBoxes
      .map(_.bytes)
      .reduce(_ ++ _)
  } else {
    Array[Byte]()
  }) ++
    Longs.toByteArray(timestamp) ++
    Longs.toByteArray(fee)

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


sealed abstract class ContractTransaction extends BifrostTransaction {

  lazy val feeBoxIdKeyPairs: IndexedSeq[(Array[Byte], PublicKey25519Proposition)] = preFeeBoxes.toIndexedSeq
    .flatMap {
      case (prop, v) =>
        v.map {
          case (nonce, _) => (PublicKeyNoncedBox.idFromBox(prop, nonce), prop)
        }
    }
  lazy val commonJson: Json = Map(
    "txHash" -> Base58.encode(id).asJson,
    "parties" -> parties.map(kv => Base58.encode(kv._1.pubKeyBytes) -> kv._2.toString).asJson,
    "signatures" -> signatures.map { case (prop, sig) => Base58.encode(prop.pubKeyBytes) -> Base58.encode(sig.bytes)
      .asJson
    }.asJson,
    "feePreBoxes" -> preFeeBoxes.map { case (prop: PublicKey25519Proposition, preBoxes: IndexedSeq[(Nonce, Long)]) =>
      Base58.encode(prop.pubKeyBytes) -> preBoxes.map(pb =>
        Map(
          "nonce" -> pb._1.toString.asJson,
          "value" -> pb._2.toString.asJson
        ).asJson
      )
    }.asJson,
    "fees" -> fees.map { case (prop, amount) => Base58.encode(prop.pubKeyBytes) -> amount.asJson }.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson
  lazy val feeBoxUnlockers: IndexedSeq[BoxUnlocker[PublicKey25519Proposition]] = feeBoxIdKeyPairs
    .map {
      case (boxId: Array[Byte], owner: PublicKey25519Proposition) =>
        new BoxUnlocker[PublicKey25519Proposition] {
          override val closedBoxId: Array[Byte] = boxId
          override val boxKey: Signature25519 = signatures.get(owner) match {
            case Some(sig) => sig
            case None => Signature25519(Array[Byte]())
          }
        }
    }
  override val fee: Long = fees.values.sum

  def parties: Map[PublicKey25519Proposition, Role]

  def signatures: Map[PublicKey25519Proposition, Signature25519]

  def preFeeBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]]

  def fees: Map[PublicKey25519Proposition, Long]

  def deductedFeeBoxes(hashNoNonces: Array[Byte]): IndexedSeq[PolyBox] = {
    val canSend = preFeeBoxes.mapValues(_.map(_._2).sum)
    val preboxesLessFees: IndexedSeq[(PublicKey25519Proposition, Long)] = canSend
      .toIndexedSeq
      .map { case (prop, amount) => prop -> (amount - fees(prop)) }

    preboxesLessFees.zipWithIndex
      .map {
        case ((prop, value), idx) =>
          val nonce = ContractTransaction
            .nonceFromDigest(
              FastCryptographicHash("ContractCreation".getBytes
                ++ prop.pubKeyBytes
                ++ hashNoNonces
                ++ Ints.toByteArray(idx)))

          PolyBox(prop, nonce, value)
      }
  }
}

object ContractTransaction {
  type PTS = Map[PublicKey25519Proposition, Role]
  type SIG = Map[PublicKey25519Proposition, Signature25519]
  type FBX = Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]]
  type F = Map[PublicKey25519Proposition, Long]
  type RP = Map[String, String]

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(8))

  def commonValidation(tx: ContractTransaction): Try[Unit] = Try {

    /* Check for no-overflow and non-negativity of fees*/
    tx.fees.values.foreach(v => require(v >= 0, "There was a negative fee"))
    require(tx.fees.values.sum >= 0, "Fees did not sum to a positive value")

    /* Check for no-overflow and non-negativity of polys */
    require(tx.preFeeBoxes.forall { case (prop, preBoxes) =>
      preBoxes.map(_._2).sum >= 0 && preBoxes.forall(_._2 >= 0)
    },
      "There were negative polys provided or the sum was negative"
    )

    /* Check that fee is covered */
    require(tx.preFeeBoxes.forall { case (prop, preBoxes) => tx.fees.get(prop) match {
      case Some(fee) => preBoxes.map(_._2).sum >= fee
      case None => false
    }
    },
      "There was an insufficient amount of polys provided to cover the fees"
    )

    require(tx.timestamp >= 0, "The timestamp was invalid")
  }

  def commonDecode(rawParties: Map[String, String],
                   rawSignatures: RP,
                   rawFeeBoxes: Map[String, IndexedSeq[(Long, Long)]],
                   rawFees: Map[String, Long]): (PTS, SIG, FBX, F) = {
    val parties = rawParties.map { case (key, value) => (BifrostTransaction.stringToPubKey(key), Role.withName(value)) }
    val signatures = rawSignatures.map { case (key, value) =>
      if (value == "") {
        (BifrostTransaction.stringToPubKey(key), Signature25519(Array.fill(Curve25519.SignatureLength)(1.toByte)))
      } else {
        (BifrostTransaction.stringToPubKey(key), BifrostTransaction.stringToSignature(value))
      }
    }
    val preFeeBoxes = rawFeeBoxes.map { case (key, value) => (BifrostTransaction.stringToPubKey(key), value) }
    val fees = rawFees.map { case (key, value) => (BifrostTransaction.stringToPubKey(key), value) }
    (parties, signatures, preFeeBoxes, fees)
  }
}

object Role extends Enumeration {
  type Role = Value
  val Producer: Role = Value("producer")
  val Investor: Role = Value("investor")
  val Hub: Role = Value("hub")
}

/**
  *
  * @param agreement          the Agreement object containing the terms for the proposed contract
  * @param preInvestmentBoxes a list of box nonces corresponding to the PolyBoxes to be used to fund the investment
  * @param parties            a mapping specifying which public key should correspond with which role for this contract
  * @param signatures         a mapping specifying the signatures by each public key for this transaction
  * @param preFeeBoxes        a mapping specifying box nonces and amounts corresponding to the PolyBoxes to be used to
  *                           pay fees for each party contributing fees
  * @param fees               a mapping specifying the amount each party is contributing to the fees
  * @param timestamp          the timestamp of this transaction
  */
case class ContractCreation(agreement: Agreement,
                            preInvestmentBoxes: IndexedSeq[(Nonce, Long)],
                            parties: Map[PublicKey25519Proposition, Role],
                            signatures: Map[PublicKey25519Proposition, Signature25519],
                            preFeeBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
                            fees: Map[PublicKey25519Proposition, Long],
                            timestamp: Long,
                            data: String)
  extends ContractTransaction {

  override type M = ContractCreation

  lazy val proposition = MofNProposition(1, parties.map(_._1.pubKeyBytes).toSet)

  //val allInvestorsSorted = parties.filter(_._2 == Role.Investor).toSeq.sortBy(_._1.pubKeyBytes.toString)


  lazy val investmentBoxIds: IndexedSeq[Array[Byte]] =
    preInvestmentBoxes.map(n => {
      //      println(parties.head._1)
      //      println(s">>>>>>>>>>>>  ${n._1}")
      //      println(s">>>>>>>>>>>> preInvestmentBoxes: ${PublicKeyNoncedBox.idFromBox(parties.head._1, n._1)}")
      PublicKeyNoncedBox.idFromBox(parties.head._1, n._1)
    })

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = investmentBoxIds ++ feeBoxIdKeyPairs.map(_._1)

  override lazy val unlockers: Traversable[BoxUnlocker[ProofOfKnowledgeProposition[PrivateKey25519]]] = investmentBoxIds
    .map(id =>
      new BoxUnlocker[PublicKey25519Proposition] {
        override val closedBoxId: Array[Byte] = id
        override val boxKey: Signature25519 = signatures(parties.head._1)
      }
    ) ++ feeBoxUnlockers

  lazy val hashNoNonces = FastCryptographicHash(
    AgreementCompanion.toBytes(agreement) ++
      parties.toSeq.sortBy(_._2).foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes) ++
      unlockers.map(_.closedBoxId).foldLeft(Array[Byte]())(_ ++ _) ++
      //boxIdsToOpen.foldLeft(Array[Byte]())(_ ++ _) ++
      fees.foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2)))

  override lazy val newBoxes: Traversable[BifrostBox] = {
    val digest = FastCryptographicHash(MofNPropositionSerializer.toBytes(proposition) ++ hashNoNonces)
    val nonce = ContractTransaction.nonceFromDigest(digest)

    val boxValue: Json = Map(
      "parties" -> parties.map(kv => Base58.encode(kv._1.pubKeyBytes) -> kv._2.toString).asJson,
      "agreement" -> agreement.json,
      "lastUpdated" -> timestamp.asJson
    ).asJson

    val investor = parties.head
    //    println(s">>>>>>> find investor: ${investor.toString()}")
    val investorProp = investor._1
    val availableBoxes: Set[(Nonce, Long)] = (preFeeBoxes(investorProp) ++ preInvestmentBoxes).toSet
    val canSend = availableBoxes.map(_._2).sum
    val polyInvestment = BigInt(
      (agreement.core.state \\ "initialCapital")
        .head
        .as[String] match {
        case Right(initialCapital: String) => initialCapital
        case Left(f: DecodingFailure) => throw f
      })
      .toLong

    val leftOver: Long = canSend - fees(investorProp) - polyInvestment.toLong
    val boxNonce = ContractTransaction.nonceFromDigest(
      FastCryptographicHash("ContractCreation".getBytes
        ++ investorProp.pubKeyBytes
        ++ hashNoNonces
        ++ Ints.toByteArray(0))
    )
    val investorDeductedBoxes = PolyBox(investorProp, boxNonce, leftOver)
    val nonInvestorDeductedBoxes = deductedFeeBoxes(hashNoNonces).filter(_.proposition != investorProp)

    IndexedSeq(ContractBox(proposition, nonce, boxValue)) ++ nonInvestorDeductedBoxes :+ investorDeductedBoxes
  }

  lazy val json: Json = (commonJson.asObject.get.toMap ++ Map(
    "preInvestmentBoxes" -> preInvestmentBoxes.map(_.asJson).asJson,
    "agreement" -> agreement.json
  )).asJson

  override lazy val serializer = ContractCreationCompanion

  //  println("BifrostTransaction")
  //println(AgreementCompanion.toBytes(agreement).mkString(""))
  //println(parties.toSeq.sortBy(_._1.pubKeyBytes.toString).foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes).mkString(""))
  //  println(investmentBoxIds.foldLeft(Array[Byte]())(_ ++ _).mkString(""))
  //  println(preInvestmentBoxes)
  //println(feeBoxIdKeyPairs.map(_._1).foldLeft(Array[Byte]())(_ ++ _).mkString(""))

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    AgreementCompanion.toBytes(agreement),
    parties.toSeq.sortBy(_._1.pubKeyBytes.mkString("")).foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes),
    unlockers.toArray.flatMap(_.closedBoxId),
    data.getBytes
    //boxIdsToOpen.foldLeft(Array[Byte]())(_ ++ _)
  )

  //  println()
  //  println(s">>>>>>>>>>> messageToSign direct: " + messageToSign.mkString(""))
  //  println()

  override def toString: String = s"ContractCreation(${json.noSpaces})"
}

object ContractCreation {

  def validate(tx: ContractCreation): Try[Unit] = Try {

    val outcome = Agreement.validate(tx.agreement)
    require(outcome.isSuccess)


    require((tx.parties.size == tx.signatures.size) && tx.parties.size >= 2,
      "There aren't exactly 3 parties involved in signing")
    require(tx.parties.size >= 2, "There aren't exactly 3 roles") // Make sure there are exactly 3 unique roles
    require(tx.parties.forall { case (proposition, _) =>
      tx.signatures(proposition).isValid(proposition, tx.messageToSign)
    }, "Not all signatures were valid")

  }.flatMap(_ => ContractTransaction.commonValidation(tx))

  implicit val decodeContractCreation: Decoder[ContractCreation] = (c: HCursor) => for {
    agreement <- c.downField("agreement").as[Agreement]
    preInvestmentBoxes <- c.downField("preInvestmentBoxes").as[IndexedSeq[(Nonce, Long)]]
    rawParties <- c.downField("parties").as[Map[String, String]]
    rawSignatures <- c.downField("signatures").as[Map[String, String]]
    rawPreFeeBoxes <- c.downField("preFeeBoxes").as[Map[String, IndexedSeq[(Long, Long)]]]
    rawFees <- c.downField("fees").as[Map[String, Long]]
    timestamp <- c.downField("timestamp").as[Long]
    data <- c.downField("data").as[String]
  } yield {
    val commonArgs = ContractTransaction.commonDecode(rawParties, rawSignatures, rawPreFeeBoxes, rawFees)
    ContractCreation(agreement,
      preInvestmentBoxes,
      commonArgs._1,
      commonArgs._2,
      commonArgs._3,
      commonArgs._4,
      timestamp,
      data)
  }

}

case class ContractMethodExecution(contractBox: ContractBox,
                                   methodName: String,
                                   parameters: Json,
                                   parties: Map[PublicKey25519Proposition, Role],
                                   signatures: Map[PublicKey25519Proposition, Signature25519],
                                   preFeeBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
                                   fees: Map[PublicKey25519Proposition, Long],
                                   timestamp: Long,
                                   data: String)
  extends ContractTransaction {

  override type M = ContractMethodExecution

  lazy val contract: Contract = {
    val valueObject: Map[String, Json] = contractBox.json
      .asObject
      .map(_.toMap)
      .get

    val cursor: HCursor = valueObject("value").hcursor
    val time = cursor.fields
    val timeUpdatedContract = {

      //      println(s"valueObject value: ${valueObject("value")}")
      //      println(s"valueObject: ${valueObject}")
      //      println(s"${contractBox.json.asObject.map(b => b.toMap)}")
      //      println(s"contractBox.json: ${cursor.downN(1).downField("value").as[Json]}")

      cursor.downField("lastUpdated").withFocus(x => timestamp.asJson).top.get

      /*contractBox.json
        .asObject
        .flatMap(_.apply("value"))
        .flatMap(_.asObject)
        .map(_.toMap)
        .get + ("lastUpdated" -> timestamp.asJson)*/
    }

    Contract(timeUpdatedContract.asJson, contractBox.id)
  }

  lazy val proposition = MofNProposition(1, contract.parties.map(p => p._1.pubKeyBytes).toSet)

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq(contractBox.id) ++ feeBoxIdKeyPairs.map(_._1)

  override lazy val unlockers: Traversable[BoxUnlocker[ProofOfKnowledgeProposition[PrivateKey25519]]] = Seq(
    new BoxUnlocker[MofNProposition] {
      override val closedBoxId: Array[Byte] = contractBox.id
      override val boxKey: Proof[MofNProposition] = MultiSignature25519(parties.map(p => signatures.get(p._1) match {
        case Some(sig) => sig
        case None => Signature25519(Array[Byte]())
      }).toSet)
    }
  ) ++ feeBoxUnlockers

  lazy val hashNoNonces = FastCryptographicHash(
    contractBox.id ++
      methodName.getBytes ++
      parties.toSeq.sortBy(_._1.pubKeyBytes.mkString("")).foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes) ++
      parameters.noSpaces.getBytes ++
      unlockers.flatMap(_.closedBoxId) ++
      Longs.toByteArray(timestamp) ++
      fees.flatMap { case (prop, value) => prop.pubKeyBytes ++ Longs.toByteArray(value) }
  )

  override lazy val newBoxes: Traversable[BifrostBox] = {
    val digest = FastCryptographicHash(MofNPropositionSerializer.toBytes(proposition) ++ hashNoNonces)
    val nonce = ContractTransaction.nonceFromDigest(digest)

    val contractResult = Contract.execute(contract, methodName)(parties.toIndexedSeq(0)._1)(parameters.asObject
      .get) match {
      case Success(res) => res match {
        case Left(updatedContract) => ContractBox(
          proposition,
          nonce,
          updatedContract.json)
        case Right(_) => contractBox
      }
      case Failure(_) => contractBox
    }

    //Handle boxes being sent from the contract to a public key
    val boxesFromContract: Option[BifrostBox] = methodName match {
      case "assetTransfer" => {
        val key = (parameters \\ "publicKey").head.asString.get
        val asset = (parameters \\ "asset").head.asString.get
        val amount = (parameters \\ "amount").head.asNumber.get.toLong.get
        if (key != "contract") {
          Some(AssetBox(PublicKey25519Proposition(key.getBytes),
            assetNonce(PublicKey25519Proposition(key.getBytes), hashNoNonces), amount, asset, parties.head._1, data))
        }
        else
          None
      }

      case "polyTransfer" => {
        val key = (parameters \\ "publicKey").head.asString.get
        val amount = (parameters \\ "amount").head.asNumber.get.toLong.get
        if (key != "contract") {
          Some(PolyBox(PublicKey25519Proposition(key.getBytes),
            assetNonce(PublicKey25519Proposition(key.getBytes), hashNoNonces), amount))
        }
        else
          None
      }

      case _ => None
    }

    if (boxesFromContract.nonEmpty)
      IndexedSeq(contractResult) ++ deductedFeeBoxes(hashNoNonces) :+ boxesFromContract.get
    else
      IndexedSeq(contractResult) ++ deductedFeeBoxes(hashNoNonces)
  }

  lazy val json: Json = (commonJson.asObject.get.toMap ++ Map(
    "contractBox" -> newBoxes.filter(b => b.isInstanceOf[ContractBox]).head.json,
    "methodName" -> methodName.asJson,
    "methodParams" -> parameters
  )).asJson

  override lazy val serializer = ContractMethodExecutionCompanion

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    FastCryptographicHash(contractBox.value.noSpaces.getBytes ++ hashNoNonces),
    data.getBytes
  )

  override def toString: String = s"ContractMethodExecution(${json.noSpaces})"
}

object ContractMethodExecution {

  def validate(tx: ContractMethodExecution): Try[Unit] = Try {

    require(tx.parties forall { case (proposition, _) =>
      tx.signatures(proposition).isValid(proposition, tx.messageToSign) &&
        MultiSignature25519(Set(tx.signatures(proposition))).isValid(tx.contractBox.proposition, tx.messageToSign)
    }, "Either an invalid signature was submitted or the party listed was not part of the contract.")

    require(tx.parties.size == 1, "An incorrect number (not equal to 1) of parties provided signatures.")

    val effDate = tx.contract.getFromContract("contractEffectiveTime")
    val expDate = tx.contract.getFromContract("contractExpirationTime")

    require(tx.timestamp >= effDate.get.asNumber.get.toLong.get, "The contract was not in effect yet.")

    require(tx.timestamp < expDate.get.asNumber.get.toLong.get, "The contract has expired.")

  }.flatMap(_ => ContractTransaction.commonValidation(tx))

  implicit val decodeContractMethodExecution: Decoder[ContractMethodExecution] = (c: HCursor) => for {
    contractBox <- c.downField("contractBox").as[ContractBox]
    methodName <- c.downField("methodName").as[String]
    methodParams <- c.downField("methodParams").as[Json]
    rawParties <- c.downField("parties").as[Map[String, String]]
    rawSignatures <- c.downField("signatures").as[Map[String, String]]
    rawPreFeeBoxes <- c.downField("preFeeBoxes").as[Map[String, IndexedSeq[(Long, Long)]]]
    rawFees <- c.downField("fees").as[Map[String, Long]]
    timestamp <- c.downField("timestamp").as[Long]
    data <- c.downField("data").as[String]
  } yield {
    val commonArgs = ContractTransaction.commonDecode(rawParties, rawSignatures, rawPreFeeBoxes, rawFees)
    ContractMethodExecution(contractBox,
      methodName,
      methodParams,
      commonArgs._1,
      commonArgs._2,
      commonArgs._3,
      commonArgs._4,
      timestamp,
      data)
  }
}

case class ContractCompletion(contractBox: ContractBox,
                              producerReputation: IndexedSeq[ReputationBox],
                              parties: Map[PublicKey25519Proposition, Role],
                              signatures: Map[PublicKey25519Proposition, Signature25519],
                              preFeeBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
                              fees: Map[PublicKey25519Proposition, Long],
                              timestamp: Long,
                              data: String)
  extends ContractTransaction {

  import ContractCompletion._

  override type M = ContractCompletion
  override lazy val bloomTopics: Option[IndexedSeq[Array[Byte]]] = Option(
    IndexedSeq("ContractCompletion".getBytes) ++ parties.map(_._1.pubKeyBytes)
  )
  lazy val contract = Contract(contractBox.json.asObject.get.apply("value").get, contractBox.id)

  lazy val proposition = MofNProposition(1, contract.parties.map(_._1.pubKeyBytes).toSet)

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq(contractBox.id) ++ feeBoxIdKeyPairs.map(_._1) //++ producerReputation.map(_.id)

  override lazy val unlockers: Traversable[BoxUnlocker[ProofOfKnowledgeProposition[PrivateKey25519]]] = Seq(
    new BoxUnlocker[MofNProposition] {
      override val closedBoxId: Array[Byte] = contractBox.id
      override val boxKey: Proof[MofNProposition] = MultiSignature25519(signatures.values.toSet)
    }
  ) ++
    //    boxIdsToOpen.tail.take(producerReputation.length).map(id =>
    //      new BoxUnlocker[PublicKey25519Proposition] {
    //        override val closedBoxId: Array[Byte] = id
    //        override val boxKey: Signature25519 = signatures(parties.find(_._2 == Role.Producer).get._1)
    //      }
    //    ) ++
    feeBoxUnlockers

  lazy val hashNoNonces = FastCryptographicHash(
    contractBox.id ++
      //producerReputation.foldLeft(Array[Byte]())((concat, box) => concat ++ box.id) ++
      parties.toSeq.sortBy(_._1.pubKeyBytes.mkString("")).foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes) ++
      unlockers.map(_.closedBoxId).foldLeft(Array[Byte]())(_ ++ _) ++
      //boxIdsToOpen.foldLeft(Array[Byte]())(_ ++ _) ++
      Longs.toByteArray(contract.lastUpdated) ++
      fees.foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2))
  )

  override lazy val newBoxes: Traversable[BifrostBox] = {
    val digest = FastCryptographicHash(MofNPropositionSerializer.toBytes(proposition) ++ hashNoNonces)
    val nonce = ContractTransaction.nonceFromDigest(digest)

    val assetCode: String = contract.getFromContract("assetCode").get.noSpaces
    val issuer: PublicKey25519Proposition = contract.parties.find(_._2 == "issuer").get._1

    val partyAssets: IndexedSeq[AssetBox] = contract.parties.map { p =>
      AssetBox(p._1, assetNonce(p._1, hashNoNonces), 0, assetCode, issuer, data)
    }.toIndexedSeq

    IndexedSeq(
      ReputationBox(parties.find(_._2 == Role.Producer).get._1, nonce, (0, 0))
    ) ++
      partyAssets ++
      deductedFeeBoxes(hashNoNonces)
  }

  lazy val json: Json = (commonJson.asObject.get.toMap ++ Map(
    "contractBox" -> contractBox.json
  )).asJson

  override lazy val serializer = ContractCompletionCompanion

  override lazy val messageToSign: Array[Byte] = Bytes.concat(hashNoNonces, data.getBytes)

  override def toString: String = s"ContractCompletion(${json.noSpaces})"

}

object ContractCompletion {

  def validate(tx: ContractCompletion): Try[Unit] = Try {
    if (tx.signatures.size != tx.parties.size) {
      throw new Exception("Inappropriate number of parties signed the completion")
    }

    if (!tx.parties.forall { case (proposition, _) =>
      val sig = Set(tx.signatures(proposition))
      val multiSig = MultiSignature25519(sig)
      val first = tx.signatures(proposition).isValid(proposition, tx.messageToSign)
      val second = multiSig.isValid(tx.contractBox.proposition, tx.messageToSign)

      first && second
    }) {
      throw new TransactionValidationException("Not all party signatures were valid")
    }

  }.flatMap(_ => ContractTransaction.commonValidation(tx))

  implicit val decodeContractCompletion: Decoder[ContractCompletion] = (c: HCursor) => for {
    contractBox <- c.downField("contractBox").as[ContractBox]
    reputationBoxes <- c.downField("reputationBoxes").as[IndexedSeq[ReputationBox]]
    rawParties <- c.downField("parties").as[Map[String, String]]
    rawSignatures <- c.downField("signatures").as[Map[String, String]]
    rawPreFeeBoxes <- c.downField("preFeeBoxes").as[Map[String, IndexedSeq[(Long, Long)]]]
    rawFees <- c.downField("fees").as[Map[String, Long]]
    timestamp <- c.downField("timestamp").as[Long]
    data <- c.downField("data").as[String]
  } yield {
    val commonArgs = ContractTransaction.commonDecode(rawParties, rawSignatures, rawPreFeeBoxes, rawFees)
    ContractCompletion(contractBox, reputationBoxes, commonArgs._1, commonArgs._2, commonArgs._3, commonArgs._4, timestamp, data)
  }

  def assetNonce(prop: PublicKey25519Proposition, hashNoNonces: Array[Byte]): Nonce = ContractTransaction
    .nonceFromDigest(
      FastCryptographicHash("ContractCompletion".getBytes
        ++ prop.pubKeyBytes
        ++ hashNoNonces)
    )

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
          val typeFilteredBoxes: Seq[BifrostPublic25519NoncedBox] = txType match {
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

          val from: IndexedSeq[(PrivateKey25519, Long, Long)] = typeFilteredBoxes
            .flatMap {
              b: BifrostPublic25519NoncedBox =>
                w.secretByPublicImage(b.proposition)
                  .map((_, b.nonce, b.value))
            }
            .toIndexedSeq

          val canSend = from.map(_._3).sum

          val updatedBalance: (PublicKey25519Proposition, Long) = typeFilteredBoxes.head match {
            case b: BifrostPublic25519NoncedBox =>
              publicKeyToSendChangeTo match {
                case "" => (b.proposition, canSend - amount - fee)
                case _ => (PublicKey25519Proposition(Base58.decode(publicKeyToSendChangeTo).get), canSend - amount - fee)
              }
            case _ => null
          }

          val to: IndexedSeq[(PublicKey25519Proposition, Long)] = IndexedSeq(updatedBalance, (recipient, amount))

          require(from.map(_._3).sum - to.map(_._2).sum == fee)
          (a._1 ++ from, a._2 ++ to)
      }
  }

  def validateTx(tx: TransferTransaction): Try[Unit] = Try {
    require(tx.from.size == tx.signatures.size)
    require(tx.to.forall(_._2 > 0L))
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
    require(tx.from.zip(tx.signatures).forall {
      case ((prop, _), proof) =>
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

  override def toString: String = s"PolyTransfer(${json.noSpaces})"
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

  override def toString: String = s"ArbitTransfer(${json.noSpaces})"
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

  override def toString: String = s"AssetTransfer(${json.noSpaces})"
}

case class TokenExchangeTransaction(buyOrder: BuySellOrder,
                                    sellOrder: BuySellOrder,
                                    override val fee: Long,
                                    override val timestamp: Long)
  extends BifrostTransaction {

  override type M = TokenExchangeTransaction
  lazy val tokenCodes = IndexedSeq(buyOrder.token1.tokenCode, buyOrder.token2.tokenCode)
  override lazy val serializer = TokenExchangeTransactionCompanion

  lazy val token1Tx: AssetTransfer = {
    val fromSeller = sellOrder.inputBoxes.map(noncedBox =>
      (PublicKey25519Proposition(noncedBox.publicKey.toByteArray), noncedBox
        .nonce)).toIndexedSeq
    val toBuyer = IndexedSeq((PublicKey25519Proposition(buyOrder.publicKey.toByteArray), sellOrder.token1.quantity))
      .toIndexedSeq
    val issuer = PublicKey25519Proposition(sellOrder.token1.tokenHub.get.toByteArray)
    val assetCode = sellOrder.token1.tokenCode

    AssetTransfer(fromSeller,
      toBuyer,
      sellOrder.signatures.map(s => Signature25519(s.toByteArray)).toIndexedSeq,
      issuer,
      assetCode,
      0L,
      sellOrder.timestamp,
      "")
  }

  lazy val token2Tx: PolyTransfer = {
    val fromBuyer = buyOrder
      .inputBoxes
      .map(noncedBox => (PublicKey25519Proposition(noncedBox.publicKey.toByteArray), noncedBox.nonce))
      .toIndexedSeq

    val toSeller = IndexedSeq(
      (PublicKey25519Proposition(sellOrder.publicKey.toByteArray), buyOrder.token2.quantity - fee)
    ).toIndexedSeq

    // TODO: Assume token2 is always Poly for now
    PolyTransfer(fromBuyer,
      toSeller,
      buyOrder.signatures.map(s => Signature25519(s.toByteArray)).toIndexedSeq,
      fee,
      buyOrder.timestamp,
      "")
  }

  override lazy val newBoxes: Traversable[BifrostBox] = token1Tx.newBoxes ++ token2Tx.newBoxes

  override lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = token1Tx.boxIdsToOpen ++ token2Tx.boxIdsToOpen

  override lazy val unlockers: Traversable[BoxUnlocker[ProofOfKnowledgeProposition[PrivateKey25519]]] = token1Tx
    .unlockers ++ token2Tx.unlockers

  override lazy val json: Json = tokenCodes.asJson

  override lazy val messageToSign: Array[Byte] = TokenExchangeTransaction
    .messageToSign(buyOrder) ++ TokenExchangeTransaction.messageToSign(sellOrder)
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
  val redemptionGroup: Map[ByteArrayWrapper, Signature25519] = availableToRedeem.flatMap(entry =>
    entry._2
      .map(t => ByteArrayWrapper(
        PublicKeyNoncedBox
          .idFromBox(t._1,
            t._2))).zip(
      signatures(entry._1))
  )
}

object PolyTransfer extends TransferUtil {

  def create(w: BWallet, toReceive: IndexedSeq[(PublicKey25519Proposition, Long)], fee: Long, data: String, publicKeyToSendFrom: Vector[String] = Vector(), publicKeyToSendChangeTo: String = "") = Try {
    val params = parametersForCreate(w, toReceive, fee, "PolyTransfer", publicKeyToSendFrom, publicKeyToSendChangeTo)
    val timestamp = Instant.now.toEpochMilli
    PolyTransfer(params._1.map(t => t._1 -> t._2), params._2, fee, timestamp, data)
  }

  def apply(from: IndexedSeq[(PrivateKey25519, Nonce)],
            to: IndexedSeq[(PublicKey25519Proposition, Value)],
            fee: Long,
            timestamp: Long,
            data: String): PolyTransfer = {
    val params = parametersForApply(from, to, fee, timestamp, "PolyTransfer", data).get
    PolyTransfer(params._1, to, params._2, fee, timestamp, data)
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

object ArbitTransfer extends TransferUtil {

  def create(w: BWallet, toRecieve: IndexedSeq[(PublicKey25519Proposition, Long)], fee: Long, data: String,
             publicKeyToSendFrom: Vector[String] = Vector(), publicKeyToSendChangeTo: String = ""): Try[ArbitTransfer] = Try {

    val params = parametersForCreate(w, toRecieve, fee, "ArbitTransfer",
      publicKeyToSendFrom, publicKeyToSendChangeTo)

    val timestamp = Instant.now.toEpochMilli

    ArbitTransfer(params._1.map(t => t._1 -> t._2), params._2, fee, timestamp, data)
  }

  def apply(from: IndexedSeq[(PrivateKey25519, Nonce)],
            to: IndexedSeq[(PublicKey25519Proposition, Value)],
            fee: Long,
            timestamp: Long,
            data: String): ArbitTransfer = {
    val params = parametersForApply(from, to, fee, timestamp, "ArbitTransfer", data).get
    ArbitTransfer(params._1, to, params._2, fee, timestamp, data)
  }

  def validate(tx: ArbitTransfer): Try[Unit] = validateTx(tx)
}

object AssetTransfer extends TransferUtil {

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

  def validate(tx: AssetTransfer): Try[Unit] = validateTx(tx)
}

object TokenExchangeTransaction {
  def validate(tx: TokenExchangeTransaction): Try[Unit] = Try {
    require(tx.fee <= tx.buyOrder.token2.quantity)

    require(tx.buyOrder.token1.tokenCode == tx.sellOrder.token1.tokenCode)
    require(tx.buyOrder.token2.tokenCode == tx.sellOrder.token2.tokenCode)
    require(tx.buyOrder.token1.quantity == tx.sellOrder.token1.quantity)
    require(tx.buyOrder.token2.quantity == tx.sellOrder.token2.quantity)
    require(Seq(tx.buyOrder, tx.sellOrder).forall(order => order.token1.quantity >= 0L))
    require(Seq(tx.buyOrder, tx.sellOrder).forall(order => order.token2.quantity >= 0L))
    require(tx.buyOrder.token1.tokenHub.get equals tx.sellOrder.token1.tokenHub.get)

    require(tx.buyOrder.inputBoxes.forall(box => box.typeOfBox == "Poly"))
    require(tx.sellOrder.inputBoxes.forall(box => box.typeOfBox == "Asset"))
    require(tx.buyOrder.isBuyOrder, "TokenExchangeTX does not have a buy order")
    require(!tx.sellOrder.isBuyOrder, "TokenExchangeTX does not have a sell order")
    require(tx.buyOrder.inputBoxes.size == tx.buyOrder.signatures.size)
    require(tx.sellOrder.inputBoxes.size == tx.sellOrder.signatures.size)

    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
    require(tx.buyOrder.inputBoxes.zip(tx.buyOrder.signatures).forall { case (box, proof) =>
      Signature25519(proof.toByteArray).isValid(PublicKey25519Proposition(box.publicKey.toByteArray), messageToSign(tx
        .buyOrder))
    })
    require(tx.sellOrder.inputBoxes.zip(tx.sellOrder.signatures).forall { case (box, proof) =>
      Signature25519(proof.toByteArray).isValid(PublicKey25519Proposition(box.publicKey.toByteArray), messageToSign(tx
        .sellOrder))
    })
  }

  def messageToSign(order: BuySellOrder): Array[Byte] = {
    "TokenExchangeTransaction".getBytes ++ order.toByteArray
  }
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
