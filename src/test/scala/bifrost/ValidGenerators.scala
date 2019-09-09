package bifrost

import java.util.UUID

import bifrost.program._
import bifrost.transaction.bifrostTransaction.BifrostTransaction.{Nonce, Value}
import bifrost.transaction.bifrostTransaction
import bifrost.transaction.box._
import com.google.common.primitives.{Bytes, Longs}
import io.circe.syntax._
import org.scalacheck.Gen
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.transaction.account.PublicKeyNoncedBox
import bifrost.transaction.bifrostTransaction.{AssetRedemption, _}
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.serialization.ExecutionBuilderCompanion
import bifrost.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.util.{Failure, Random, Success, Try}

/**
  * Created by cykoz on 5/11/2017.
  */
trait ValidGenerators extends BifrostGenerators {

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

  lazy val validProgramGen: Gen[Program] = for {
    producer <- propositionGen
    investor <- propositionGen
    hub <- propositionGen
    executionBuilder <- validExecutionBuilderGen().map(_.json)
    id <- genBytesList(FastCryptographicHash.DigestSize)
  } yield {
    Program(Map(
      "parties" -> Map(
      Base58.encode(producer.pubKeyBytes) -> "producer",
      Base58.encode(investor.pubKeyBytes) -> "investor",
      Base58.encode(hub.pubKeyBytes) -> "hub"
      ).asJson,
      "executionBuilder" -> executionBuilder,
      "lastUpdated" -> System.currentTimeMillis().asJson
    ).asJson, id)
  }

  lazy val validProgramCreationGen: Gen[ProgramCreation] = for {
    executionBuilder <- validExecutionBuilderGen()
    timestamp <- positiveLongGen
    numInvestmentBoxes <- positiveTinyIntGen
    data <- stringGen
    maxFee <- positiveTinyIntGen
  } yield {
    Try {
      val senderKeyPair = keyPairSetGen.sample.get.head
      val sender = senderKeyPair._2

      val preInvestmentBoxes: IndexedSeq[(Nonce, Long)] = (0 until numInvestmentBoxes)
        .map { _ =>
          sampleUntilNonEmpty(positiveLongGen) -> (sampleUntilNonEmpty(positiveLongGen) / 1e5.toLong + 1L)
        }

      val state =
        s"""
           |{ "a": 0 }
         """.stripMargin.asJson

      val stateTwo =
        s"""
           |{ "b": 0 }
         """.stripMargin.asJson

      val stateThree =
        s"""
           |{ "c": 0 }
         """.stripMargin.asJson

      val stateBox = StateBox(sender, 0L, null, state)
      val stateBoxTwo = StateBox(sender, 1L, null, stateTwo)
      val stateBoxThree = StateBox(sender, 2L, null, stateThree)

      val readOnlyUUIDs = Seq(UUID.nameUUIDFromBytes(stateBoxTwo.id), UUID.nameUUIDFromBytes(stateBoxThree.id))

      val codeBox = CodeBox(sender, 3L, null, Seq("add = function() { a = 2 + 2 }"), Map("add" -> Seq("Number", "Number")))

      val investmentBoxIds: IndexedSeq[Array[Byte]] = preInvestmentBoxes
        .map(n => PublicKeyNoncedBox.idFromBox(sender, n._1))

      val feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]] =
        Map(sender -> IndexedSeq(preFeeBoxGen(0L, maxFee).sample.get))


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
        ExecutionBuilderCompanion.toBytes(executionBuilder),
        sender.pubKeyBytes,
        (investmentBoxIds ++ feeBoxIdKeyPairs.map(_._1)).reduce(_ ++ _),
        data.getBytes
      )

      val signature = Map(sender -> PrivateKey25519Companion.sign(senderKeyPair._1, messageToSign))

      ProgramCreation(
        executionBuilder,
        readOnlyUUIDs,
        preInvestmentBoxes,
        sender,
        signature,
        feePreBoxes,
        fees,
        timestamp,
        data
      )
    } match {
      case Success(s) => s
      case Failure(e) => throw e.getCause
    }
  }

  lazy val validProgramMethods: List[String] = List("add")

  /*def createProgramBox(executionBuilder: ExecutionBuilder, parties: Map[PublicKey25519Proposition, Role.Role]): ProgramBox = {

    val programJson = Map(
      "parties" -> parties.map(kv => Base58.encode(kv._1.pubKeyBytes) -> kv._2.toString).asJson,
      "executionBuilder" -> executionBuilder.json,
      "lastUpdated" -> System.currentTimeMillis().asJson
    ).asJson

    val program = Program(programJson, sampleUntilNonEmpty(genBytesList(FastCryptographicHash.DigestSize)))

    val proposition = MofNProposition(1, parties.map(_._1.pubKeyBytes).toSet)
    ProgramBox(proposition, sampleUntilNonEmpty(positiveLongGen), program.json)
  }*/

  lazy val semanticallyValidProgramMethodExecutionGen: Gen[ProgramMethodExecution] = for {
    timestamp <- positiveLongGen.map(_ / 3)
    data <- stringGen
  } yield {
    val senderKeyPair = keyPairSetGen.sample.get.head
    val sender = senderKeyPair._2

    /* TODO: Don't know why this re-sampling is necessary here -- but should figure that out */
    var executionBuilderOpt = validExecutionBuilderGen().sample
    while (executionBuilderOpt.isEmpty) executionBuilderOpt = validExecutionBuilderGen().sample
    val executionBuilder = executionBuilderOpt.get

    val methodName = "add" //sampleUntilNonEmpty(Gen.oneOf(executionBuilder.core.registry.keys.toSeq))


    val state = Map("a" -> "0").asJson

    val stateBox = StateBox(sender, 0L, UUID.nameUUIDFromBytes(StateBox.idFromBox(sender, 0L)), state)
    val codeBox = CodeBox(sender, 1L,  UUID.nameUUIDFromBytes(CodeBox.idFromBox(sender, 1L)),
      Seq("add = function() { a = 2 + 2 }"), Map("add" -> Seq("Number", "Number")))


    val stateUUID: UUID = UUID.nameUUIDFromBytes(stateBox.id)
//    val proposition = MofNProposition(1, parties.map(_.pubKeyBytes).toSet)
    val executionBox = ExecutionBox(sender, 2L, UUID.nameUUIDFromBytes(ExecutionBox.idFromBox(sender, 2L)), Seq(stateUUID), Seq(codeBox.id))


    val boxAmounts: Seq[Long] = splitAmongN(sampleUntilNonEmpty(positiveLongGen),
      sampleUntilNonEmpty(positiveTinyIntGen),
      minShareSize = 0) match {
      case Success(amounts) => amounts
      case f: Failure[_] => throw f.exception
    }

    val feeBoxes: Seq[(Nonce, Long)] = boxAmounts
      .map { boxAmount => sampleUntilNonEmpty(preFeeBoxGen(boxAmount, boxAmount)) }

    val feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Nonce)]] =
      Map(sender -> feeBoxes.toIndexedSeq)

    val feeBoxIdKeyPairs: IndexedSeq[(Array[Byte], PublicKey25519Proposition)] = feePreBoxes.toIndexedSeq
      .flatMap {
        case (prop, v) =>
          v.map {
            case (nonce, _) => (PublicKeyNoncedBox.idFromBox(prop, nonce), prop)
          }
      }

    val senderFeePreBoxes = feePreBoxes(sender)
    val fees = Map(sender -> senderFeePreBoxes.map(_._2).sum)

    /*val parameters = executionBuilder
      .core
      .registry
      .keys
      .map(k => k -> "".asJson)
      .toMap
      .asJson
     */

    val parameters = {}.asJson

    val hashNoNonces = FastCryptographicHash(
      executionBox.id ++
        methodName.getBytes ++
        sender.pubKeyBytes ++
        parameters.noSpaces.getBytes ++
        (executionBox.id ++ feeBoxIdKeyPairs.flatMap(_._1)) ++
        Longs.toByteArray(timestamp) ++
        fees.flatMap { case (prop, value) => prop.pubKeyBytes ++ Longs.toByteArray(value) }
    )

    val messageToSign = Bytes.concat(FastCryptographicHash(executionBox.bytes ++ hashNoNonces), data.getBytes)
    val signature = Map(sender -> PrivateKey25519Companion.sign(senderKeyPair._1, messageToSign))

    bifrostTransaction.ProgramMethodExecution(
      Seq(stateBox),
      Seq(codeBox),
      executionBox,
      methodName,
      parameters,
      sender,
      signature,
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
    issuer <- keyPairSetGen
    assetCode <- stringGen
    data <- stringGen
  } yield {
    val toKeyPairs = sampleUntilNonEmpty(keyPairSetGen).head
    val to = IndexedSeq((toKeyPairs._2, 4L))

    val oneHub = issuer.head

    val fakeSigs = IndexedSeq(Signature25519(Array()))

    val messageToSign = AssetCreation(to, Map(), assetCode, oneHub._2, fee, timestamp, data).messageToSign

    val signatures = Map(oneHub._2 -> PrivateKey25519Companion.sign(oneHub._1, messageToSign))

    AssetCreation(to, signatures, assetCode, oneHub._2, fee, timestamp, data)
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
