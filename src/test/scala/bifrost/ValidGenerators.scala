package bifrost

import bifrost.blocks.BifrostBlock
import bifrost.contract.Contract.Status.Status
import bifrost.contract._
import bifrost.transaction.bifrostTransaction.BifrostTransaction.{Nonce, Value}
import bifrost.transaction.bifrostTransaction.Role.Role
import bifrost.transaction.{bifrostTransaction, _}
import bifrost.transaction.box.proposition.MofNProposition
import bifrost.transaction.box._
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.syntax._
import org.scalacheck.Gen
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.transaction.account.PublicKeyNoncedBox
import bifrost.transaction.bifrostTransaction.{AssetRedemption, _}
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.serialization.AgreementCompanion
import bifrost.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.util.{Failure, Random, Success, Try}

/**
  * Created by cykoz on 5/11/2017.
  */
trait ValidGenerators extends BifrostGenerators {

  val validStatuses: List[Status] = Contract.Status.values.toList

  private val POSSIBLE_ROLES = Seq(Role.Producer, Role.Investor, Role.Hub)

  lazy val validBifrostTransactionSeqGen: Gen[Seq[BifrostTransaction]] = for {
    seqLen <- positiveMediumIntGen
  } yield {
    0 until seqLen map {
      _ => {
        val g: Gen[BifrostTransaction] = sampleUntilNonEmpty(Gen.oneOf(transactionTypes))
        sampleUntilNonEmpty(g)
      }
    }
  }

  lazy val validContractGen: Gen[Contract] = for {
    producer <- propositionGen
    investor <- propositionGen
    hub <- propositionGen
    storage <- jsonGen()
    status <- Gen.oneOf(validStatuses)
    agreement <- validAgreementGen().map(_.json)
    id <- genBytesList(FastCryptographicHash.DigestSize)
  } yield {
    Contract(Map(
      "parties" -> Map(
      Base58.encode(producer.pubKeyBytes) -> "producer",
      Base58.encode(investor.pubKeyBytes) -> "investor",
      Base58.encode(hub.pubKeyBytes) -> "hub"
      ).asJson,
      "storage" -> Map("status" -> status.toString.asJson, "other" -> storage).asJson,
      "agreement" -> agreement,
      "lastUpdated" -> System.currentTimeMillis().asJson
    ).asJson, id)
  }

  lazy val validContractCreationGen: Gen[ContractCreation] = for {
    agreement <- validAgreementGen()
    timestamp <- positiveLongGen
    numInvestmentBoxes <- positiveTinyIntGen
    data <- stringGen
  } yield {
    Try {
      val allKeyPairs = (0 until 3).map(_ => sampleUntilNonEmpty(keyPairSetGen).head)
      val parties = allKeyPairs.map(_._2)

      val preInvestmentBoxes: IndexedSeq[(Nonce, Long)] = (0 until numInvestmentBoxes)
        .map { _ =>
          sampleUntilNonEmpty(positiveLongGen) -> (sampleUntilNonEmpty(positiveLongGen) / 1e5.toLong + 1L)
        }

      val partiesWithRoles: Map[PublicKey25519Proposition, Role.Role] = Map(allKeyPairs.head._2 -> Role.Investor) ++
        allKeyPairs.drop(1).map(_._2)
        .zip((Stream continually Random.shuffle(List(Role.Producer, Role.Hub))).flatten)
        .map(t => t._1 -> t._2)

      //val allInvestorsSorted = partiesWithRoles.filter(_._2 == Role.Investor).toSeq.sortBy(_._1.pubKeyBytes.toString)

      val stateBox = StateBox(parties.head, 0L, Seq("var a = 0"), true)

      val codeBox = CodeBox(parties.head, 1L, Seq("add = function() { a = 2 + 2 }"))

      val investmentBoxIds: IndexedSeq[Array[Byte]] = preInvestmentBoxes
        .map(n => PublicKeyNoncedBox.idFromBox(parties(0), n._1))

      val feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]] = {
        val sum = sampleUntilNonEmpty(Gen.choose(0L, Long.MaxValue))

        splitAmongN(sum, parties.length, minShareSize = 0) match {
          case Success(shares) => parties
            .zip(shares)
            .map {
              case (party, share) =>
                party -> (splitAmongN(share, sampleUntilNonEmpty(positiveTinyIntGen), minShareSize = 0) match {
                  case Success(boxAmounts) => boxAmounts
                  case f: Failure[_] => throw f.exception
                })
                  .map { boxAmount => sampleUntilNonEmpty(preFeeBoxGen(boxAmount, boxAmount)) }
                  .toIndexedSeq
            }
            .toMap

          case f: Failure[_] => throw f.exception
        }
      }


      val feeBoxIdKeyPairs: IndexedSeq[(Array[Byte], PublicKey25519Proposition)] = feePreBoxes.toIndexedSeq
        .flatMap { case (prop, v) =>
          v.map {
            case (nonce, _) => (PublicKeyNoncedBox.idFromBox(prop, nonce), prop)
          }
        }

      val fees = feePreBoxes.map { case (prop, preBoxes) =>
        prop -> preBoxes.map(_._2).sum
      }


      val messageToSign = Bytes.concat(
        AgreementCompanion.toBytes(agreement),
        partiesWithRoles.toSeq.sortBy(_._1.pubKeyBytes.mkString("")).foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes),
        (investmentBoxIds ++ feeBoxIdKeyPairs.map(_._1)).reduce(_ ++ _),
        data.getBytes
      )

      val signatures = allKeyPairs.map {
        keypair =>
          val sig = PrivateKey25519Companion.sign(keypair._1, messageToSign)
          (keypair._2, sig)
      }

      ContractCreation(
        agreement,
        preInvestmentBoxes,
        partiesWithRoles,
        signatures.toMap,
        feePreBoxes,
        fees,
        timestamp,
        data
      )
    } match {
      case Success(s) => s
      case Failure(e) => println("FAIL"); throw e
    }
  }

  lazy val validContractMethods: List[String] = List("add")

  def createContractBox(agreement: ExecutionBuilder, parties: Map[PublicKey25519Proposition, Role.Role]): ContractBox = {

    val contractJson = Map(
      "parties" -> parties.map(kv => Base58.encode(kv._1.pubKeyBytes) -> kv._2.toString).asJson,
      "agreement" -> agreement.json,
      "lastUpdated" -> System.currentTimeMillis().asJson
    ).asJson

    val contract = Contract(contractJson, sampleUntilNonEmpty(genBytesList(FastCryptographicHash.DigestSize)))

    val proposition = MofNProposition(1, parties.map(_._1.pubKeyBytes).toSet)
    ContractBox(proposition, sampleUntilNonEmpty(positiveLongGen), contract.json)
  }

  lazy val semanticallyValidContractMethodExecutionGen: Gen[ContractMethodExecution] = for {
    timestamp <- positiveLongGen.map(_ / 3)
    data <- stringGen
  } yield {
    val nrOfParties = 3 //Random.nextInt(1022) + 2
    val allKeyPairs = (0 until nrOfParties).map(_ => sampleUntilNonEmpty(keyPairSetGen).head)
    val parties = allKeyPairs.map(_._2)
    val roles = (0 until nrOfParties).map(_ => Random.shuffle(POSSIBLE_ROLES).head)

    /* TODO: Don't know why this re-sampling is necessary here -- but should figure that out */
    var agreementOpt = validAgreementGen().sample
    while (agreementOpt.isEmpty) agreementOpt = validAgreementGen().sample
    val agreement = agreementOpt.get

    val contractBox = createContractBox(agreement, parties.zip(roles).toMap)

    val stateBox = StateBox(parties.head, 0L, Seq("var a = 0"), true)
    val codeBox = CodeBox(parties.head, 1L, Seq("add = function() { a = 2 + 2 }"))

    val methodName = sampleUntilNonEmpty(Gen.oneOf(agreement.core.registry.keys.toSeq))

    val sender: (Role, (PrivateKey25519, PublicKey25519Proposition)) =
      sampleUntilNonEmpty(Gen.oneOf(roles.zip(allKeyPairs)))

    val boxAmounts: Seq[Long] = splitAmongN(sampleUntilNonEmpty(positiveLongGen),
      sampleUntilNonEmpty(positiveTinyIntGen),
      minShareSize = 0) match {
      case Success(amounts) => amounts
      case f: Failure[_] => throw f.exception
    }

    val feeBoxes: Seq[(Nonce, Long)] = boxAmounts
      .map { boxAmount => sampleUntilNonEmpty(preFeeBoxGen(boxAmount, boxAmount)) }

    val feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Nonce)]] =
      Map(sender._2._2 -> feeBoxes.toIndexedSeq)

    val feeBoxIdKeyPairs: IndexedSeq[(Array[Byte], PublicKey25519Proposition)] = feePreBoxes.toIndexedSeq
      .flatMap {
        case (prop, v) =>
          v.map {
            case (nonce, _) => (PublicKeyNoncedBox.idFromBox(prop, nonce), prop)
          }
      }

    val senderFeePreBoxes = feePreBoxes(sender._2._2)
    val fees = Map(sender._2._2 -> senderFeePreBoxes.map(_._2).sum)

    val parameters = agreement
      .core
      .registry
      .keys
      .map(k => k -> "".asJson)
      .toMap
      .asJson

    val hashNoNonces = FastCryptographicHash(
      contractBox.id ++
        methodName.getBytes ++
        sender._2._2.pubKeyBytes ++
        parameters.noSpaces.getBytes ++
        (contractBox.id ++ feeBoxIdKeyPairs.flatMap(_._1)) ++
        Longs.toByteArray(timestamp) ++
        fees.flatMap { case (prop, value) => prop.pubKeyBytes ++ Longs.toByteArray(value) }
    )

    val messageToSign = Bytes.concat(FastCryptographicHash(contractBox.value.noSpaces.getBytes ++ hashNoNonces), data.getBytes)
    val signature = PrivateKey25519Companion.sign(sender._2._1, messageToSign)

    bifrostTransaction.ContractMethodExecution(
      contractBox,
      stateBox,
      codeBox,
      methodName,
      parameters,
      Map(sender._2._2 -> sender._1),
      Map(sender._2._2 -> signature),
      feePreBoxes,
      fees,
      timestamp,
      data)
  }


  lazy val validPolyTransferGen: Gen[PolyTransfer] = for {
    from <- fromSeqGen
    to <- toSeqGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    data <- stringGen
  } yield {
    val fromKeyPairs = sampleUntilNonEmpty(keyPairSetGen).head
    val from = IndexedSeq((fromKeyPairs._1, Longs.fromByteArray(FastCryptographicHash("Testing").take(8))))
    val toKeyPairs = sampleUntilNonEmpty(keyPairSetGen).head
    val to = IndexedSeq((toKeyPairs._2, 4L))

    PolyTransfer(from, to, fee, timestamp, data)
  }

  private val testingValue: Value = Longs
    .fromByteArray(FastCryptographicHash("Testing")
      .take(Longs.BYTES))

  lazy val validArbitTransferGen: Gen[ArbitTransfer] = for {
    _ <- fromSeqGen
    _ <- toSeqGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    data <- stringGen
  } yield {
    val fromKeyPairs = sampleUntilNonEmpty(keyPairSetGen).head
    val from = IndexedSeq((fromKeyPairs._1, testingValue))
    val toKeyPairs = sampleUntilNonEmpty(keyPairSetGen).head
    val to = IndexedSeq((toKeyPairs._2, 4L))

    ArbitTransfer(from, to, fee, timestamp, data)
  }

  lazy val validCoinbaseTransactionGen: Gen[CoinbaseTransaction] = for {
    _ <- toSeqGen
    timestamp <- positiveLongGen
    id <- modifierIdGen
  } yield {
    val toKeyPairs = sampleUntilNonEmpty(keyPairSetGen).head
    val to = IndexedSeq((toKeyPairs._2, 4L))
    val fakeSigs = IndexedSeq(Signature25519(Array()))
    val messageToSign = CoinbaseTransaction(
      to,
      fakeSigs,
      timestamp,
      id).messageToSign
    // sign with own key because coinbase is literally giving yourself money
    val signatures = IndexedSeq(PrivateKey25519Companion.sign(toKeyPairs._1, messageToSign))
    CoinbaseTransaction(to, signatures, timestamp, id)
  }

  lazy val validAssetTransferGen: Gen[AssetTransfer] = for {
    _ <- fromSeqGen
    _ <- toSeqGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    hub <- propositionGen
    assetCode <- stringGen
    data <- stringGen
  } yield {
    val fromKeyPairs = sampleUntilNonEmpty(keyPairSetGen).head
    val from = IndexedSeq((fromKeyPairs._1, testingValue))
    val toKeyPairs = sampleUntilNonEmpty(keyPairSetGen).head
    val to = IndexedSeq((toKeyPairs._2, 4L))

    AssetTransfer(from, to, hub, assetCode, fee, timestamp, data)
  }

  lazy val validAssetCreationGen: Gen[AssetCreation] = for {
    _ <- toSeqGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    hub <- keyPairSetGen
    assetCode <- stringGen
    data <- stringGen
  } yield {
    val toKeyPairs = sampleUntilNonEmpty(keyPairSetGen).head
    val to = IndexedSeq((toKeyPairs._2, 4L))

    val oneHub = hub.head

//    val hashNoNonces = FastCryptographicHash(
//      to.map(_._1.pubKeyBytes).reduce(_ ++ _) ++
//        Longs.toByteArray(timestamp) ++
//        Longs.toByteArray(fee)
//    )
//
//    val newBoxes: Traversable[BifrostBox] = to.zipWithIndex.map {
//      case ((prop, value), idx) =>
//        val nonce = AssetCreation.nonceFromDigest(FastCryptographicHash(
//          "AssetCreation".getBytes ++
//            prop.pubKeyBytes ++
//            oneHub._2.pubKeyBytes ++
//            assetCode.getBytes ++
//            hashNoNonces ++
//            Ints.toByteArray(idx)
//        ))
//        AssetBox(prop, nonce, value, assetCode, oneHub._2)
//    }
//
//    val commonMessageToSign: Array[Byte] = (if (newBoxes.nonEmpty) {
//      newBoxes
//        .map(_.bytes)
//        .reduce(_ ++ _)
//    } else {
//      Array[Byte]()
//    }) ++
//      Longs.toByteArray(timestamp) ++
//      Longs.toByteArray(fee)
//
//    val messageToSign: Array[Byte] = Bytes.concat(
//      "AssetCreation".getBytes(),
//      commonMessageToSign,
//      oneHub._2.pubKeyBytes,
//      assetCode.getBytes
//    )

    val fakeSigs = IndexedSeq(Signature25519(Array()))

    val messageToSign = AssetCreation(to, fakeSigs, assetCode, oneHub._2, fee, timestamp, data).messageToSign

    val signatures = IndexedSeq(PrivateKey25519Companion.sign(oneHub._1, messageToSign))

    AssetCreation(to, signatures, assetCode, oneHub._2, fee, timestamp, data)

    //println("Generated")
  }

  lazy val validProfileTransactionGen: Gen[ProfileTransaction] = for {
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield {
    val rnd = new Random
    val keyValues = Map(
      "role" -> ProfileBox.acceptableRoleValues.toVector(rnd.nextInt(ProfileBox.acceptableRoleValues.size))
    )
    val fromKeyPairs = sampleUntilNonEmpty(keyPairSetGen).head
    val from = fromKeyPairs._2
    val signature = PrivateKey25519Companion
      .sign(fromKeyPairs._1, ProfileTransaction.messageToSign(timestamp, from, keyValues))

    ProfileTransaction(from, signature, keyValues, fee, timestamp)
  }

  lazy val validAssetRedemptionGen: Gen[AssetRedemption] = for {
    assetLength <- positiveTinyIntGen
    hub <- propositionGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    data <- stringGen
  } yield {
    val assets = (0 until assetLength).map { _ => sampleUntilNonEmpty(stringGen) }

    val fromKeyPairs: IndexedSeq[(PublicKey25519Proposition, PrivateKey25519)] = keyPairSetGen
      .sample
      .get
      .map(kp => kp._2 -> kp._1)
      .toIndexedSeq

    val availableToRedeem: Map[String, IndexedSeq[(PublicKey25519Proposition, Nonce)]] = assets
      .map(_ -> (0 until sampleUntilNonEmpty(positiveTinyIntGen))
        .map { _ =>
          sampleUntilNonEmpty(Gen.oneOf(fromKeyPairs))._1 ->
            sampleUntilNonEmpty(Gen.choose(Long.MinValue, Long.MaxValue))
        })
      .toMap

    val toKeyPairs = keyPairSetGen
      .sample
      .get
      .toIndexedSeq

    val remainderAllocations: Map[String, IndexedSeq[(PublicKey25519Proposition, Long)]] = assets
      .map(_ -> (0 until sampleUntilNonEmpty(positiveTinyIntGen))
        .map { _ =>
          sampleUntilNonEmpty(Gen.oneOf(toKeyPairs))._2 -> sampleUntilNonEmpty(positiveMediumIntGen).toLong
        })
      .toMap

    val dummySigs = availableToRedeem
      .map(entry => entry._1 -> entry._2
        .map(_ => Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte))))

    val dummyTx = AssetRedemption(availableToRedeem, remainderAllocations, dummySigs, hub, fee, timestamp, data)

    val fromKeyMap = fromKeyPairs.toMap
    val signatures = availableToRedeem
      .map {
        case (assetId, boxes) =>
          assetId -> boxes.map(b => PrivateKey25519Companion.sign(fromKeyMap(b._1), dummyTx.messageToSign))
      }

    dummyTx.copy(signatures = signatures)
  }
}
