package bifrost

import java.time.Instant

import bifrost.contract.Contract.Status
import bifrost.contract.Contract.Status.Status
import bifrost.contract._
import bifrost.transaction.ContractTransaction.Nonce
import bifrost.transaction.PolyTransfer.Nonce
import bifrost.transaction._
import bifrost.transaction.box.proposition.MofNProposition
import bifrost.transaction.box.{ContractBox, ProfileBox, ReputationBox}
import bifrost.transaction.proof.MultiSignature25519
import com.google.common.primitives.{Bytes, Longs}
import io.circe.Json
import org.scalacheck.Gen
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.crypto.encode.Base58
import io.circe.syntax._
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.{Proof, Signature25519}
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.crypto.signatures.Curve25519

import scala.util.{Failure, Random, Success}

/**
  * Created by cykoz on 5/11/2017.
  */
trait ValidGenerators extends BifrostGenerators {

  val validStatuses: List[Status] = Contract.Status.values.toList

  lazy val validContractGen: Gen[Contract] = for {
    producer <- propositionGen
    investor <- propositionGen
    hub <- propositionGen
    storage <- jsonGen()
    status <- Gen.oneOf(validStatuses)
    agreement <- validAgreementGen.map(_.json)
    id <- genBytesList(FastCryptographicHash.DigestSize)
  } yield Contract(Map(
    "producer" -> Base58.encode(producer.pubKeyBytes).asJson,
    "investor" -> Base58.encode(investor.pubKeyBytes).asJson,
    "hub" -> Base58.encode(hub.pubKeyBytes).asJson,
    "storage" -> Map("status" -> status.toString.asJson, "other" -> storage).asJson,
    "agreement" -> agreement,
    "lastUpdated" -> System.currentTimeMillis().asJson
  ).asJson, id)

  lazy val validContractCreationGen: Gen[ContractCreation] = for {
    agreement <- validAgreementGen
    timestamp <- positiveLongGen
    numInvestmentBoxes <- positiveTinyIntGen
  } yield {
    val allKeyPairs = (0 until 3).map(_ => keyPairSetGen.sample.get.head)
    val parties = allKeyPairs.map(_._2)
    val roles = List(Role.Investor, Role.Producer, Role.Hub)

    val preInvestmentBoxes: IndexedSeq[(Nonce, Long)] = (0 until numInvestmentBoxes).map { _ =>
      positiveLongGen.sample.get -> (positiveLongGen.sample.get/1e5.toLong + 1L)
    }

    val investmentBoxIds: IndexedSeq[Array[Byte]] = preInvestmentBoxes.map(n => PublicKeyNoncedBox.idFromBox(parties(0), n._1))

    val feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]] = {
      val sum = Gen.choose(0L, Long.MaxValue).sample.get

      splitAmongN(sum, parties.length, minShareSize = 0) match {
        case Success(shares) => parties.zip(shares).map { case (party, share) =>
          party -> (splitAmongN(share, positiveTinyIntGen.sample.get, minShareSize = 0) match {
            case Success(boxAmounts) => boxAmounts
            case f: Failure[_] => throw f.exception
          }).map { boxAmount => preFeeBoxGen(boxAmount, boxAmount).sample.get }.toIndexedSeq
        }.toMap

        case f: Failure[_] => throw f.exception
      }
    }

    println(feePreBoxes)

    val feeBoxIdKeyPairs: IndexedSeq[(Array[Byte], PublicKey25519Proposition)] = feePreBoxes.toIndexedSeq.flatMap { case (prop, v) =>
      v.map {
        case (nonce, value) => (PublicKeyNoncedBox.idFromBox(prop, nonce), prop)
      }
    }

    val fees = feePreBoxes.map { case (prop, preBoxes) =>
      prop -> preBoxes.map(_._2).sum
    }

    val messageToSign = Bytes.concat(
      AgreementCompanion.toBytes(agreement),
      roles.zip(parties).sortBy(_._1).foldLeft(Array[Byte]())((a, b) => a ++ b._2.pubKeyBytes),
      (investmentBoxIds ++ feeBoxIdKeyPairs.map(_._1)).reduce(_ ++ _)
    )

    val signatures = allKeyPairs.map{
      keypair =>
        val sig = PrivateKey25519Companion.sign(keypair._1, messageToSign)
        (keypair._2, sig)
    }

    ContractCreation(
      agreement,
      preInvestmentBoxes,
      roles.zip(parties).toMap,
      signatures.toMap,
      feePreBoxes,
      fees,
      timestamp
    )
  }

  lazy val validContractMethods: List[String] = List("endorseCompletion", "currentStatus", "deliver", "confirmDelivery", "checkExpiration")

  def createContractBox(agreement: Agreement,
                        status: Status,
                        currentFulfillment: Map[String, Json],
                        currentEndorsement: Map[String, Json],
                        parties: Map[Role.Role, PublicKey25519Proposition]): ContractBox = {

    val contract = Contract(Map(
      "producer" -> Base58.encode(parties(Role.Producer).pubKeyBytes).asJson,
      "investor" -> Base58.encode(parties(Role.Investor).pubKeyBytes).asJson,
      "hub" -> Base58.encode(parties(Role.Hub).pubKeyBytes).asJson,
      "storage" -> Map(
        "status" -> status.asJson,
        "currentFulfillment" -> currentFulfillment.asJson,
        "endorsements" -> currentEndorsement.asJson,
        "other" -> jsonGen().sample.get
      ).asJson,
      "agreement" -> agreement.json,
      "lastUpdated" -> System.currentTimeMillis().asJson
    ).asJson, genBytesList(FastCryptographicHash.DigestSize).sample.get)

    val proposition = MofNProposition(1, parties.map(_._2.pubKeyBytes).toSet)
    ContractBox(proposition, positiveLongGen.sample.get, contract.json)
  }

  lazy val semanticallyValidContractMethodExecutionGen: Gen[ContractMethodExecution] = for {
    methodName <- Gen.oneOf(validContractMethods)
    parameters <- jsonGen()
    timestamp <- positiveLongGen.map(_ / 3)
    deliveredQuantity <- positiveLongGen
    effDelta <- positiveLongGen.map(_ / 3)
    expDelta <- positiveLongGen.map(_ / 3)
  } yield {
    val allKeyPairs = (0 until 3).map(_ => keyPairSetGen.sample.get.head)
    val parties = allKeyPairs.map(_._2)
    val roles = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub))

    val currentFulfillment = Map("deliveredQuantity" -> deliveredQuantity.asJson)
    val currentEndorsement = Map[String, Json]()

    val contractBox = createContractBox(
      Agreement(validAgreementTermsGen.sample.get, stringGen.sample.get, timestamp - effDelta, timestamp + expDelta),
      Status.INITIALISED,
      currentFulfillment,
      currentEndorsement,
      roles.zip(parties).toMap
    )

    val sender = Gen.oneOf(Seq(Role.Producer, Role.Investor , Role.Hub).zip(allKeyPairs)).sample.get

    val feePreBoxes = Map(sender._2._2 -> (splitAmongN(positiveLongGen.sample.get, positiveTinyIntGen.sample.get, minShareSize = 0) match {
      case Success(boxAmounts) => boxAmounts
      case f: Failure[_] => throw f.exception
    }).map { boxAmount => preFeeBoxGen(boxAmount, boxAmount).sample.get }.toIndexedSeq)


    val feeBoxIdKeyPairs: IndexedSeq[(Array[Byte], PublicKey25519Proposition)] = feePreBoxes.toIndexedSeq.flatMap { case (prop, v) =>
      v.map {
        case (nonce, value) => (PublicKeyNoncedBox.idFromBox(prop, nonce), prop)
      }
    }

    val senderFeePreBoxes = feePreBoxes(sender._2._2)
    val fees = Map(sender._2._2 -> senderFeePreBoxes.map(_._2).sum)

    val hashNoNonces = FastCryptographicHash(
      contractBox.id ++
        methodName.getBytes ++
        sender._2._2.pubKeyBytes ++
        parameters.noSpaces.getBytes ++
        (contractBox.id ++ feeBoxIdKeyPairs.flatMap(_._1)) ++
        Longs.toByteArray(timestamp) ++
        fees.flatMap{ case (prop, value) => prop.pubKeyBytes ++ Longs.toByteArray(value) }
    )

    val messageToSign = FastCryptographicHash(contractBox.value.asObject.get("storage").get.noSpaces.getBytes ++ hashNoNonces)
    val signature = PrivateKey25519Companion.sign(sender._2._1, messageToSign)

    ContractMethodExecution(
      contractBox,
      methodName,
      parameters,
      Map(sender._1 -> sender._2._2),
      Map(sender._2._2 -> signature),
      feePreBoxes,
      fees,
      timestamp
    )
  }

  lazy val validContractCompletionGen: Gen[ContractCompletion] = for {
    timestamp <- positiveLongGen
    agreement <- validAgreementGen
    status <- Gen.oneOf(validStatuses)
    deliveredQuantity <- positiveLongGen
    numReputation <- positiveTinyIntGen
  } yield {
    val allKeyPairs = (0 until 3).map(_ => keyPairSetGen.sample.get.head)
    val parties = allKeyPairs.map(_._2)
    val roles = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub))

    val currentFulfillment = Map("deliveredQuantity" -> deliveredQuantity.asJson)
    val currentEndorsement = parties.map(p =>
      Base58.encode(p.pubKeyBytes) -> Base58.encode(FastCryptographicHash(currentFulfillment.asJson.noSpaces.getBytes)).asJson
    ).toMap

    val contractBox = createContractBox(agreement, status, currentFulfillment, currentEndorsement, roles.zip(parties).toMap)

    val contract = Contract(contractBox.json.asObject.get.apply("value").get, contractBox.id)

    val feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]] = {
      val sum = positiveLongGen.sample.get

      splitAmongN(sum, parties.length, minShareSize = 0, maxShareSize = Long.MaxValue) match {
        case Success(shares) => parties.zip(shares).map { case (party, share) =>
          party -> (splitAmongN(share, positiveTinyIntGen.sample.get, minShareSize = 0) match {
            case Success(boxAmounts) => boxAmounts
            case f: Failure[_] => throw f.exception
          }).map { boxAmount => preFeeBoxGen(boxAmount, boxAmount).sample.get }.toIndexedSeq
        }.toMap

        case f: Failure[_] => throw f.exception
      }
    }

    println(feePreBoxes)

    val feeBoxIdKeyPairs: IndexedSeq[(Array[Byte], PublicKey25519Proposition)] = feePreBoxes.toIndexedSeq.flatMap { case (prop, v) =>
      v.map {
        case (nonce, value) => (PublicKeyNoncedBox.idFromBox(prop, nonce), prop)
      }
    }

    val reasonableDoubleGen: Gen[Double] = Gen.choose(-1e3, 1e3)

    val reputation = (0 until numReputation).map(_ =>
      ReputationBox(parties(roles.indexOf(Role.Producer)), Gen.choose(Long.MinValue, Long.MaxValue).sample.get, (reasonableDoubleGen.sample.get, reasonableDoubleGen.sample.get))
    )

    val boxIdsToOpen = IndexedSeq(contractBox.id) ++ reputation.map(_.id) ++ feeBoxIdKeyPairs.map(_._1)
    val fees = feePreBoxes.map { case (prop, preBoxes) =>
      prop -> preBoxes.map(_._2).sum
    }

    val messageToSign = FastCryptographicHash(
      contractBox.id ++
        roles.zip(parties).sortBy(_._1).foldLeft(Array[Byte]())((a, b) => a ++ b._2.pubKeyBytes) ++
        boxIdsToOpen.foldLeft(Array[Byte]())(_ ++ _) ++
        Longs.toByteArray(contract.lastUpdated) ++
        fees.foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2))
    )

    val signatures = allKeyPairs.map{
      keypair =>
        val sig = PrivateKey25519Companion.sign(keypair._1, messageToSign)
        (keypair._2, sig)
    }

    ContractCompletion(
      contractBox,
      reputation,
      roles.zip(parties).toMap,
      signatures.toMap,
      feePreBoxes,
      fees,
      timestamp
    )

  }

  lazy val validPolyTransferGen: Gen[PolyTransfer] = for {
    from <- fromSeqGen
    to <- toSeqGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield {
    val fromKeyPairs = keyPairSetGen.sample.get.head
    val from = IndexedSeq((fromKeyPairs._1, Longs.fromByteArray(FastCryptographicHash("Testing").take(8))))
    val toKeyPairs = keyPairSetGen.sample.get.head
    val to = IndexedSeq((toKeyPairs._2, 4L))

    PolyTransfer(from, to, fee, timestamp)
  }

  lazy val validArbitTransferGen: Gen[ArbitTransfer] = for {
    from <- fromSeqGen
    to <- toSeqGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield {
    val fromKeyPairs = keyPairSetGen.sample.get.head
    val from = IndexedSeq((fromKeyPairs._1, Longs.fromByteArray(FastCryptographicHash("Testing").take(8))))
    val toKeyPairs = keyPairSetGen.sample.get.head
    val to = IndexedSeq((toKeyPairs._2, 4L))

    ArbitTransfer(from, to, fee, timestamp)
  }

  lazy val validProfileTransactionGen: Gen[ProfileTransaction] = for {
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield {
    val rnd = new Random
    val keyValues = Map(
      "role" -> ProfileBox.acceptableRoleValues.toVector(rnd.nextInt(ProfileBox.acceptableRoleValues.size))
    )
    val fromKeyPairs = keyPairSetGen.sample.get.head
    val from = fromKeyPairs._2
    val signature = PrivateKey25519Companion.sign(fromKeyPairs._1, ProfileTransaction.messageToSign(timestamp, from, keyValues))
    ProfileTransaction(from, signature, keyValues, fee, timestamp)
  }

  lazy val validAssetRedemptionGen: Gen[AssetRedemption] = for {
    assetLength <- positiveTinyIntGen
    hub <- propositionGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield {

    val assets = (0 until assetLength).map { _ => stringGen.sample.get }

    val fromKeyPairs: IndexedSeq[(PublicKey25519Proposition, PrivateKey25519)] = keyPairSetGen.sample.get.map(kp => kp._2 -> kp._1).toIndexedSeq
    val availableToRedeem: Map[String, IndexedSeq[(PublicKey25519Proposition, Nonce)]] = assets.map(_ -> (0 until positiveTinyIntGen.sample.get).map { _ =>
      Gen.oneOf(fromKeyPairs).sample.get._1 -> Gen.choose(Long.MinValue, Long.MaxValue).sample.get
    }).toMap

    val toKeyPairs = keyPairSetGen.sample.get.toIndexedSeq
    val remainderAllocations: Map[String, IndexedSeq[(PublicKey25519Proposition, Long)]] = assets.map(_ -> (0 until positiveTinyIntGen.sample.get).map { _ =>
      Gen.oneOf(toKeyPairs).sample.get._2 -> positiveMediumIntGen.sample.get.toLong
    }).toMap

    val dummySigs = availableToRedeem.map(entry => entry._1 -> entry._2.map(_ => Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte))) )
    val dummyTx = AssetRedemption(availableToRedeem, remainderAllocations, dummySigs, hub, fee, timestamp)

    val fromKeyMap = fromKeyPairs.toMap
    val signatures = availableToRedeem.map { case (assetId, boxes) =>
      assetId -> boxes.map(b => PrivateKey25519Companion.sign(fromKeyMap(b._1), dummyTx.messageToSign) )
    }

    dummyTx.copy(signatures = signatures)
  }

}
