package co.topl.utils

import co.topl.attestation.PublicKeyPropositionCurve25519.evProducer
import co.topl.modifier.transaction.Transaction.TX
import co.topl.modifier.transaction._
import co.topl.program._
import io.circe.syntax._
import org.scalacheck.Gen
import scorex.crypto.hash.Blake2b256

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

  lazy val validPolyTransferGen: Gen[PolyTransfer[_]] = for {
    from <- fromSeqGen
    to <- toSeqGen
    attestation <- attestationGen
    key <- publicKeyPropositionCurve25519Gen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    data <- stringGen
  } yield {

    val tx = PolyTransfer(from, to, attestation, fee, timestamp, Some(data))
    val sig = key._1.sign(tx.messageToSign)
    tx.copy(attestation = Map(key._2 -> sig))
  }

  lazy val validArbitTransferGen: Gen[ArbitTransfer[_]] = for {
    from <- fromSeqGen
    to <- toSeqGen
    attestation <- attestationGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    data <- stringGen
  } yield {

    ArbitTransfer(from, to, attestation, fee, timestamp, Some(data))
  }

  lazy val validAssetTransferGen: Gen[AssetTransfer[_]] = for {
    from <- fromSeqGen
    to <- assetToSeqGen
    attestation <- attestationGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    data <- stringGen
  } yield {

    AssetTransfer(from, to, attestation, fee, timestamp, Some(data), minting = true)
  }

  /*
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
   */
}

