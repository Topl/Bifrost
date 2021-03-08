package co.topl.utils

import co.topl.attestation.PublicKeyPropositionCurve25519.evProducer
import co.topl.attestation.keyManagement.{KeyRing, KeyfileCurve25519, PrivateKeyCurve25519}
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.consensus.genesis.PrivateGenesis
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.box.Box.identifier
import co.topl.modifier.box._
import co.topl.modifier.transaction.Transaction.TX
import co.topl.modifier.transaction._
import co.topl.nodeView.history.History
import co.topl.nodeView.state.State
import co.topl.program._
import co.topl.settings.AppSettings
import io.circe.syntax._
import org.scalacheck.Gen
import scorex.crypto.hash.Blake2b256

import scala.util.{Failure, Random, Success}

trait ValidGenerators extends CoreGenerators {


  val keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] =
    KeyRing(settings.application.keyFileDir.get, KeyfileCurve25519)

  val genesisBlock: Block = PrivateGenesis((_: Int, _: Option[String]) => {
    keyRing.generateNewKeyPairs(num = 3) match {
      case Success(keys) => keys.map(_.publicImage)
      case Failure(ex)   => throw ex
    } }, settings).getGenesisBlock.get._1

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

  def genesisState(settings: AppSettings, genesisBlockWithVersion: Block = genesisBlock): State = {
    History.readOrGenerate(settings).append(genesisBlock)
    State.genesisState(settings, Seq(genesisBlockWithVersion))
  }

  def validPolyTransfer(
                         keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
                         state: State,
                         fee: Long = 1L
                       ): Gen[PolyTransfer[PublicKeyPropositionCurve25519]] = {
    val sender = keyRing.addresses.head
    val prop = keyRing.lookupPublicKey(sender).get
    val value = SimpleValue(1)
    val recipients = IndexedSeq((sender, value))
    val rawTx = PolyTransfer.createRaw(
      state,
      recipients,
      IndexedSeq(sender),
      changeAddress = sender,
      None,
      fee,
      data = None
    ).get

    val sig = keyRing.signWithAddress(sender)(rawTx.messageToSign).get
    val tx = rawTx.copy(attestation = Map(prop -> sig))
    tx
  }

  def validArbitTransfer(
                          keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
                          state: State,
                          fee: Long = 1L
                        ): Gen[ArbitTransfer[PublicKeyPropositionCurve25519]] = {
    val sender = keyRing.addresses.head
    val prop = keyRing.lookupPublicKey(sender).get
    val value = SimpleValue(1)
    val recipients = IndexedSeq((sender, value))
    val rawTx = ArbitTransfer.createRaw(
      state,
      recipients,
      IndexedSeq(sender),
      changeAddress = sender,
      None,
      fee,
      data = None
    ).get

    val sig = keyRing.signWithAddress(sender)(rawTx.messageToSign).get
    val tx = rawTx.copy(attestation = Map(prop -> sig))
    tx
  }

  def validAssetTransfer(
                          keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
                          state: State,
                          fee: Long = 1L,
                          minting: Boolean = false
                        ): Gen[AssetTransfer[PublicKeyPropositionCurve25519]] = {

    val (sender, asset) = if(!minting) {
      val boxAmounts = sumBoxes(collectBoxes(keyRing.addresses, state))
      val sender = boxAmounts(Random.nextInt(boxAmounts.length))
      val assetAmount = Gen.chooseNum(0L, sender._2("AssetBox").longValue()).sample.get
      val asset = AssetValue(assetAmount, AssetCode(1: Byte, sender._1, "test"), SecurityRoot.empty)
      (sender._1, asset)
    } else {
      val sender = keyRing.addresses.head
      val asset = AssetValue(1, AssetCode(1: Byte, sender, "test"), SecurityRoot.empty)
      (sender, asset)
    }

    val prop = keyRing.lookupPublicKey(sender).get
    val recipients = IndexedSeq((keyRing.addresses.toSeq(Random.nextInt(keyRing.addresses.size)), asset))
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

  def collectBoxes(addresses: Set[Address], state: State): Seq[TokenBox[TokenValueHolder]] = {
    addresses.flatMap(address => state.getTokenBoxes(address)).flatten.toSeq
  }

  def sumBoxes(boxes: Seq[TokenBox[TokenValueHolder]]): Seq[(Address, Map[String, Int128])] = {
    val boxesByOwner = boxes.groupBy(_.evidence)
    val ownerQuantities = boxesByOwner.map {
      case (evidence, boxes) =>
        Address(evidence) -> boxes
          .groupBy(identifier(_).typeString)
          .map {
            case (boxType, b) =>
              boxType -> b.map(_.value.quantity).foldLeft[Int128](0)(_ + _)
          }
    }.toSeq
    ownerQuantities
  }
}

