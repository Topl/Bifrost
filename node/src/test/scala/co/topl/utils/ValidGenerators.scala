package co.topl.utils

import co.topl.attestation.PublicKeyPropositionCurve25519
import co.topl.attestation.PublicKeyPropositionCurve25519.evProducer
import co.topl.consensus.genesis.PrivateGenesis
import co.topl.keyManagement.{KeyRing, KeyfileCurve25519, PrivateKeyCurve25519}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.box.{AssetCode, AssetValue, SecurityRoot}
import co.topl.modifier.transaction.Transaction.TX
import co.topl.modifier.transaction._
import co.topl.nodeView.history.History
import co.topl.nodeView.state.State
import co.topl.program._
import co.topl.settings.AppSettings
import io.circe.syntax._
import org.scalacheck.Gen
import co.topl.crypto.hash.{Blake2b256, Hash}

import scala.util.{Failure, Success}

trait ValidGenerators extends CoreGenerators {

  val keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] =
    KeyRing(settings.application.keyFileDir.get, KeyfileCurve25519)

  keyRing.generateNewKeyPairs(num = 3) match {
    case Success(_) => ()
    case Failure(error) => throw error
  }

  val genesisBlock: Block = PrivateGenesis(keyRing.addresses, settings).getGenesisBlock.get._1

  val genesisBlockId: ModifierId = genesisBlock.id

  val state: State = genesisState(settings)

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
    id <- genBytesList(Hash[Blake2b256].digestSize)
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

  def genesisState(settings: AppSettings, genesisBlockWithVersion: Block = genesisBlock): State = {
    History.readOrGenerate(settings).append(genesisBlock)
    State.genesisState(settings, Seq(genesisBlockWithVersion))
  }

  def validAssetTransfer(
                          keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
                          state: State,
                          fee: Long = 1L,
                          minting: Boolean = false
  ): Gen[AssetTransfer[PublicKeyPropositionCurve25519]] = {
    val sender = keyRing.addresses.head
    val prop = keyRing.lookupPublicKey(sender).get
    val asset = AssetValue(1, AssetCode(1: Byte, sender, "test"), SecurityRoot.empty)
    val recipients = IndexedSeq((sender, asset))
    val rawTx = AssetTransfer.createRaw(
      state,
      recipients,
      IndexedSeq(sender),
      changeAddress = sender,
      None,
      fee,
      data = None,
      minting
    ).get

    val sig = keyRing.signWithAddress(sender)(rawTx.messageToSign).get
    val tx = rawTx.copy(attestation = Map(prop -> sig))
    tx
  }
}

