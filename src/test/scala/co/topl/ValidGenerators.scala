package co.topl

import co.topl.crypto.{FastCryptographicHash, PrivateKey25519, Signature25519}
import co.topl.modifier.transaction.Transaction.{Nonce, Value}
import co.topl.modifier.transaction._
import co.topl.nodeView.state.box.{PublicKeyNoncedBox, _}
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.program._
import com.google.common.primitives.{Bytes, Longs}
import io.circe.syntax._
import org.scalacheck.Gen
import scorex.crypto.signatures.Signature
import scorex.util.encode.Base58

import scala.util.{Failure, Success, Try}

/**
  * Created by cykoz on 5/11/2017.
  */
trait ValidGenerators extends BifrostGenerators {

  lazy val validBifrostTransactionSeqGen: Gen[Seq[Transaction]] = for {
    seqLen <- positiveMediumIntGen
  } yield {
    0 until seqLen map {
      _ => {
        val g: Gen[Transaction] = sampleUntilNonEmpty(Gen.oneOf(transactionTypes))
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

      val stateTwo =
        s"""
           |{ "b": 0 }
         """.stripMargin.asJson

      val stateThree =
        s"""
           |{ "c": 0 }
         """.stripMargin.asJson

      val stateBoxTwo = StateBox(sender, 1L, programIdGen.sample.get, stateTwo)
      val stateBoxThree = StateBox(sender, 2L, programIdGen.sample.get, stateThree)

      val readOnlyIds = Seq(stateBoxTwo.value, stateBoxThree.value)

      val feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]] =
        Map(sender -> IndexedSeq(preFeeBoxGen(0L, maxFee).sample.get))

      val fees = feePreBoxes.map { case (prop, preBoxes) =>
        prop -> preBoxes.map(_._2).sum
      }

      val falseSig = Map(sender -> Signature25519(Signature @@ Array.emptyByteArray))
      val pc = ProgramCreation(executionBuilder, readOnlyIds, preInvestmentBoxes, sender, falseSig, feePreBoxes, fees, timestamp, data)
      val signature = Map(sender -> PrivateKey25519.sign(senderKeyPair._1, pc.messageToSign))

      pc.copy(signatures = signature)
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
    sbProgramId <- programIdGen
    cbProgramId <- programIdGen
    exProgramId <- programIdGen
  } yield {
    val senderKeyPair = keyPairSetGen.sample.get.head
    val sender = senderKeyPair._2

    /* TODO: Don't know why this re-sampling is necessary here -- but should figure that out */
    var executionBuilderOpt = validExecutionBuilderGen().sample
    while (executionBuilderOpt.isEmpty) executionBuilderOpt = validExecutionBuilderGen().sample

    val methodName = "add" //sampleUntilNonEmpty(Gen.oneOf(executionBuilder.core.registry.keys.toSeq))

    val state = Map("a" -> "0").asJson

    val stateBox = StateBox(sender, 0L, sbProgramId, state)
    val codeBox = CodeBox(sender, 1L, cbProgramId, Seq("add = function() { a = 2 + 2 }"), Map("add" -> Seq("Number", "Number")))

    //    val proposition = MofNProposition(1, parties.map(_.pubKeyBytes).toSet)
    val executionBox = ExecutionBox(sender, 2L, exProgramId, Seq(stateBox.value), Seq(codeBox.value))


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
    val signature = Map(sender -> PrivateKey25519.sign(senderKeyPair._1, messageToSign))

    ProgramMethodExecution(
      executionBox,
      Seq(stateBox),
      Seq(codeBox),
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
    val fakeSigs = IndexedSeq(Signature25519(Signature @@ Array.emptyByteArray))
    val messageToSign = CoinbaseTransaction(
      to,
      fakeSigs,
      timestamp,
      id.hashBytes).messageToSign
    // sign with own key because coinbase is literally giving yourself money
    val signatures = IndexedSeq(PrivateKey25519.sign(toKeyPairs._1, messageToSign))
    CoinbaseTransaction(to, signatures, timestamp, id.hashBytes)
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

    val messageToSign = AssetCreation(to, Map(), assetCode, oneHub._2, fee, timestamp, data).messageToSign

    val signatures = Map(oneHub._2 -> PrivateKey25519.sign(oneHub._1, messageToSign))

    AssetCreation(to, signatures, assetCode, oneHub._2, fee, timestamp, data)
  }
}

