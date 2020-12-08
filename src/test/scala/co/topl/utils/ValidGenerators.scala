package co.topl.utils

import co.topl.attestation.PublicKeyPropositionCurve25519
import co.topl.modifier.transaction.Transaction.TX
import co.topl.modifier.transaction._
import co.topl.nodeView.state.box.Box.Nonce
import co.topl.nodeView.state.box.TokenBox.Value
import co.topl.program._
import com.google.common.primitives.{Bytes, Longs}
import io.circe.syntax._
import org.scalacheck.Gen
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.Signature
import scorex.util.encode.Base58

import scala.util.{Failure, Success, Try}

trait ValidGenerators extends CoreGenerators {

  lazy val validBifrostTransactionSeqGen: Gen[Seq[TX]] = for {
    seqLen <- positiveMediumIntGen
  } yield {
    0 until seqLen map {
      _ => {
        val g: Gen[TX] = sampleUntilNonEmpty(Gen.oneOf(transactionTypes))
        sampleUntilNonEmpty(g)
      }
    }
  }

  lazy val validProgramGen: Gen[Program] = for {
    producer <- propositionGen
    investor <- propositionGen
    hub <- propositionGen
    executionBuilder <- validExecutionBuilderGen().map(_.json)
    id <- genBytesList(Blake2b256.DigestSize)
  } yield {
    Program(Map(
      "parties" -> Map(
        producer.toString -> "producer",
        investor.toString -> "investor",
        hub.toString -> "hub"
      ).asJson,
      "executionBuilder" -> executionBuilder,
      "lastUpdated" -> System.currentTimeMillis().asJson
    ).asJson, id)
  }

  lazy val validProgramMethods: List[String] = List("add")

  lazy val validPolyTransferGen: Gen[PolyTransfer] = for {
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    data <- stringGen
  } yield {
    val fromKeyPairs = sampleUntilNonEmpty(keyPairSetGen).head
    val from = IndexedSeq((fromKeyPairs._1, testingValue))
    val toKeyPairs = sampleUntilNonEmpty(keyPairSetGen).head
    val to = IndexedSeq((toKeyPairs._2, 4L))

    PolyTransfer(from, to, fee, timestamp, data)
  }

  private val testingValue: Value = Longs
    .fromByteArray(Blake2b256("Testing")
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

    val signatures = Map(oneHub._2 -> oneHub._1.sign(messageToSign))

    AssetCreation(to, signatures, assetCode, oneHub._2, fee, timestamp, data)
  }
}

