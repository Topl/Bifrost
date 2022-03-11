package co.topl.utils

import co.topl.attestation.keyManagement._
import co.topl.attestation._
import co.topl.consensus.{NxtConsensus, NxtLeaderElection, ProtocolVersioner}
import co.topl.consensus.genesis.TestGenesis
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.box.Box.identifier
import co.topl.modifier.box._
import co.topl.modifier.transaction.Transaction.TX
import co.topl.modifier.transaction.builder.TransferRequests.{
  ArbitTransferRequest,
  AssetTransferRequest,
  PolyTransferRequest
}
import co.topl.modifier.transaction.builder.{BoxSelectionAlgorithms, TransferBuilder}
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}
import co.topl.nodeView.history.{BlockProcessor, History, InMemoryKeyValueStore, Storage}
import co.topl.nodeView.state.State
import co.topl.settings.{AppContext, AppSettings, StartupOpts, Version}
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.StringDataTypes.Latin1Data
import com.typesafe.config.Config
import org.scalacheck.Gen
import org.scalatest.Suite

import java.nio.file.Files
import scala.collection.immutable.ListMap
import scala.util.Random

trait TestSettings {
  implicit def settings: AppSettings = TestSettings.defaultSettings
  implicit def appContext: AppContext = TestSettings.defaultAppContext

  val protocolVersioner: ProtocolVersioner =
    ProtocolVersioner(settings.application.version, settings.forging.protocolVersions)
  val nxtLeaderElection: NxtLeaderElection = new NxtLeaderElection(protocolVersioner)
}

object TestSettings {
  private val settingsFilename = "node/src/test/resources/application-test.conf"

  val (defaultSettings: AppSettings, defaultConfig: Config) = {
    val (s, c) = AppSettings.read(StartupOpts(Some(settingsFilename)))
    s.copy(
      application = s.application.copy(
        dataDir = Some(Files.createTempDirectory("bifrost-test-data").toString),
        consensusStoreVersionsToKeep = 10
      )
    ) -> c
  }

  val defaultAppContext: AppContext =
    new AppContext(defaultSettings, StartupOpts(), None)
}

trait NodeGenerators extends CommonGenerators with DiskKeyFileTestHelper with TestSettings {
  self: Suite =>

  lazy val versionGen: Gen[Version] = for {
    first  <- Gen.choose(0: Byte, Byte.MaxValue)
    second <- Gen.choose(0: Byte, Byte.MaxValue)
    third  <- Gen.choose(0: Byte, Byte.MaxValue)
  } yield new Version(first, second, third)

  lazy val genesisBlock: Block =
    TestGenesis(keyRingCurve25519, keyRingEd25519, propsThresholdCurve25519, settings).getGenesisBlock.get._1

  def genesisBlockId: ModifierId = genesisBlock.id

  def generateHistory(genesisBlock: Block = genesisBlock): History = {
//    val dataDir = s"/tmp/bifrost/test-data/test-${Random.nextInt(10000000)}"
//
//    val iFile = new File(s"$dataDir/blocks")
//    iFile.mkdirs()
//    val blockStorage = new LDBVersionedStore(iFile, 100)

    val storage =
      new Storage(new InMemoryKeyValueStore)
    // we don't care about validation here
    val validators = Seq()

    var history = new History(storage, BlockProcessor(1024), validators)
    val consensusState = NxtConsensus.State(Int128(10000000), 1000000000000000000L, 0L, 0L)
    val consensusView = NxtConsensus.View(consensusState, nxtLeaderElection, protocolVersioner)

    history = history.append(genesisBlock, consensusView).get._1
    assert(history.modifierById(genesisBlock.id).isDefined)
    history
  }

  def genesisState(
    settings:                AppSettings,
    genesisBlockWithVersion: Block = genesisBlock,
    consensusView:           NxtConsensus.View
  ): State = {
    History.readOrGenerate(settings).append(genesisBlock, consensusView)
    State.genesisState(settings, Seq(genesisBlockWithVersion))
  }

  lazy val genesisState: State =
    genesisState(
      settings,
      genesisBlock,
      NxtConsensus.View(
        NxtConsensus.State(Int128(10000000), 1000000000000000000L, 0L, 0L),
        nxtLeaderElection,
        protocolVersioner
      )
    )

  lazy val validBifrostTransactionSeqGen: Gen[Seq[TX]] = for {
    seqLen <- positiveMediumIntGen
  } yield 0 until seqLen map { _ =>
    val g: Gen[TX] = sampleUntilNonEmpty(
      Gen.oneOf(
        validPolyTransferGen(keyRingCurve25519, keyRingEd25519, propsThresholdCurve25519, genesisState),
        validArbitTransferGen(keyRingCurve25519, keyRingEd25519, propsThresholdCurve25519, genesisState),
        validAssetTransferGen(keyRingCurve25519, keyRingEd25519, propsThresholdCurve25519, genesisState, minting = true)
      )
    )
    sampleUntilNonEmpty(g)
  }

  def validPolyTransferCurve25519Gen(
    keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    state:   State,
    fee:     Long = 1L
  ): Gen[PolyTransfer[PublicKeyPropositionCurve25519]] = {

    val availablePolys = sumBoxes(collectBoxes(keyRing.addresses, state), "PolyBox")
    val (sender, poly) = availablePolys(Random.nextInt(availablePolys.length))
    val polyAmount = Int128(sampleUntilNonEmpty(Gen.chooseNum(1L + fee, poly.longValue() - 1))) - fee

    val recipients = {
      val address: Address = keyRing.addresses.filterNot(_ == sender).toSeq(Random.nextInt(keyRing.addresses.size - 1))
      List((address, polyAmount))
    }

    val rawTx =
      TransferBuilder
        .buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519](
          state,
          PolyTransferRequest(List(sender), recipients, sender, fee, None),
          BoxSelectionAlgorithms.All
        )
        .getOrThrow()

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

  def validPolyTransferThresholdCurve25519Gen(
    keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    props:   Set[ThresholdPropositionCurve25519],
    state:   State,
    fee:     Long = 1L
  ): Gen[PolyTransfer[ThresholdPropositionCurve25519]] = {

    val addresses = props.map(_.address)
    val addressesToPropMap = props.map(prop => (prop.address, prop)).toMap

    val availablePolys: Seq[(Address, Int128)] = sumBoxes(collectBoxes(addresses, state), "PolyBox")
    val (sender, poly): (Address, Int128) = availablePolys(Random.nextInt(availablePolys.length))
    val polyAmount = Int128(sampleUntilNonEmpty(Gen.chooseNum(1L + fee, poly.longValue() - 1))) - fee

    val recipients = {
      val address: Address =
        addresses.filterNot(_ == sender).toSeq(Random.nextInt(addresses.size - 1))
      List((address, polyAmount))
    }

    val rawTx =
      TransferBuilder
        .buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519](
          state,
          PolyTransferRequest(List(sender), recipients, sender, fee, None),
          BoxSelectionAlgorithms.All
        )
        .getOrThrow()

    val signatures = keyRing.generateAttestation(keyRing.addresses)(rawTx.messageToSign).values.toSet
    val thresholdSignature = ThresholdSignatureCurve25519(signatures)
    val attestation = ListMap(addressesToPropMap(sender) -> thresholdSignature)

    rawTx.copy(attestation = attestation)
  }

  def validPolyTransferEd25519Gen(
    keyRing: KeyRing[PrivateKeyEd25519, KeyfileEd25519],
    state:   State,
    fee:     Long = 1L
  ): Gen[PolyTransfer[PublicKeyPropositionEd25519]] = {

    val availablePolys = sumBoxes(collectBoxes(keyRing.addresses, state), "PolyBox")
    val (sender, poly) = availablePolys(Random.nextInt(availablePolys.length))
    val polyAmount = Int128(sampleUntilNonEmpty(Gen.chooseNum(1L + fee, poly.longValue() - 1))) - fee

    val recipients = {
      val address: Address = keyRing.addresses.filterNot(_ == sender).toSeq(Random.nextInt(keyRing.addresses.size - 1))
      List((address, polyAmount))
    }

    val rawTx =
      TransferBuilder
        .buildUnsignedPolyTransfer[PublicKeyPropositionEd25519](
          state,
          PolyTransferRequest(List(sender), recipients, sender, fee, None),
          BoxSelectionAlgorithms.All
        )
        .getOrThrow()

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

  def validPolyTransferGen(
    keyRingCurve25519:        KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    keyRingEd25519:           KeyRing[PrivateKeyEd25519, KeyfileEd25519],
    propsThresholdCurve25519: Set[ThresholdPropositionCurve25519],
    state:                    State,
    fee:                      Long = 1L
  ): Gen[PolyTransfer[_ <: Proposition]] =
    Gen.oneOf(
      validPolyTransferCurve25519Gen(keyRingCurve25519, state, fee),
      validPolyTransferThresholdCurve25519Gen(keyRingCurve25519, propsThresholdCurve25519, state, fee),
      validPolyTransferEd25519Gen(keyRingEd25519, state, fee)
    )

  def validArbitTransferCurve25519Gen(
    keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    state:   State,
    fee:     Long = 1L
  ): Gen[ArbitTransfer[PublicKeyPropositionCurve25519]] = {

    val availableArbits = sumBoxes(collectBoxes(keyRing.addresses, state), "ArbitBox")
    val (sender, arbit) = availableArbits(Random.nextInt(availableArbits.length))
    val arbitAmount = Int128(sampleUntilNonEmpty(Gen.chooseNum(1L + fee, arbit.longValue() - 1))) - fee

    val recipients = {
      val address = keyRing.addresses.filterNot(_ == sender).toSeq(Random.nextInt(keyRing.addresses.size - 1))
      List((address, arbitAmount))
    }

    val rawTx =
      TransferBuilder
        .buildUnsignedArbitTransfer[PublicKeyPropositionCurve25519](
          state,
          ArbitTransferRequest(List(sender), recipients, sender, sender, fee, None),
          BoxSelectionAlgorithms.All
        )
        .getOrThrow()

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

  def validArbitTransferThresholdCurve25519Gen(
    keyRing:      KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    propositions: Set[ThresholdPropositionCurve25519],
    state:        State,
    fee:          Long = 1L
  ): Gen[ArbitTransfer[ThresholdPropositionCurve25519]] = {

    val addresses = propositions.map(_.address)
    val addressesToPropMap = propositions.map(prop => (prop.address, prop)).toMap

    val availableArbits = sumBoxes(collectBoxes(addresses, state), "ArbitBox")
    val (sender, arbit) = availableArbits(Random.nextInt(availableArbits.length))
    val arbitAmount = Int128(sampleUntilNonEmpty(Gen.chooseNum(1L + fee, arbit.longValue() - 1))) - fee

    val recipients = {
      val address: Address =
        addresses.filterNot(_ == sender).toSeq(Random.nextInt(addresses.size - 1))
      List((address, arbitAmount))
    }

    val rawTx =
      TransferBuilder
        .buildUnsignedArbitTransfer[ThresholdPropositionCurve25519](
          state,
          ArbitTransferRequest(List(sender), recipients, sender, sender, fee, None),
          BoxSelectionAlgorithms.All
        )
        .getOrThrow()

    val signatures = keyRing.generateAttestation(keyRing.addresses)(rawTx.messageToSign).values.toSet
    val thresholdSignature = ThresholdSignatureCurve25519(signatures)
    val attestation = ListMap(addressesToPropMap(sender) -> thresholdSignature)

    rawTx.copy(attestation = attestation)
  }

  def validArbitTransferEd25519Gen(
    keyRing: KeyRing[PrivateKeyEd25519, KeyfileEd25519],
    state:   State,
    fee:     Long = 1L
  ): Gen[ArbitTransfer[PublicKeyPropositionEd25519]] = {

    val availableArbits = sumBoxes(collectBoxes(keyRing.addresses, state), "ArbitBox")
    val (sender, arbit) = availableArbits(Random.nextInt(availableArbits.length))
    val arbitAmount = Int128(sampleUntilNonEmpty(Gen.chooseNum(1L + fee, arbit.longValue() - 1))) - fee

    val recipients = {
      val address = keyRing.addresses.filterNot(_ == sender).toSeq(Random.nextInt(keyRing.addresses.size - 1))
      List((address, arbitAmount))
    }

    val rawTx =
      TransferBuilder
        .buildUnsignedArbitTransfer[PublicKeyPropositionEd25519](
          state,
          ArbitTransferRequest(List(sender), recipients, sender, sender, fee, None),
          BoxSelectionAlgorithms.All
        )
        .getOrThrow()

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

  def validArbitTransferGen(
    keyRingCurve25519:        KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    keyRingEd25519:           KeyRing[PrivateKeyEd25519, KeyfileEd25519],
    propsThresholdCurve25519: Set[ThresholdPropositionCurve25519],
    state:                    State,
    fee:                      Long = 1L
  ): Gen[ArbitTransfer[_ <: Proposition]] =
    Gen.oneOf(
      validArbitTransferCurve25519Gen(keyRingCurve25519, state, fee),
      validArbitTransferThresholdCurve25519Gen(keyRingCurve25519, propsThresholdCurve25519, state, fee),
      validArbitTransferEd25519Gen(keyRingEd25519, state, fee)
    )

  def validAssetTransferCurve25519Gen(
    keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    state:   State,
    fee:     Long = 1L,
    minting: Boolean = false
  ): Gen[AssetTransfer[PublicKeyPropositionCurve25519]] = {
    val sender = keyRing.addresses.head
    val asset = AssetValue(1, AssetCode(1: Byte, sender, Latin1Data.unsafe("test")), SecurityRoot.empty)
    val recipients = List((sender, asset))

    val rawTx =
      TransferBuilder
        .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](
          state,
          AssetTransferRequest(List(sender), recipients, sender, sender, fee, None, minting),
          BoxSelectionAlgorithms.All
        )
        .getOrThrow()

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

  def validAssetTransferThresholdCurve25519Gen(
    keyRing:      KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    propositions: Set[ThresholdPropositionCurve25519],
    state:        State,
    fee:          Long = 1L,
    minting:      Boolean = false
  ): Gen[AssetTransfer[ThresholdPropositionCurve25519]] = {

    val addresses = propositions.map(_.address)
    val addressesToPropMap = propositions.map(prop => (prop.address, prop)).toMap

    val sender = addresses.head
    val asset = AssetValue(1, AssetCode(1: Byte, sender, Latin1Data.unsafe("test")), SecurityRoot.empty)
    val recipients = List((sender, asset))

    val rawTx =
      TransferBuilder
        .buildUnsignedAssetTransfer[ThresholdPropositionCurve25519](
          state,
          AssetTransferRequest(List(sender), recipients, sender, sender, fee, None, minting),
          BoxSelectionAlgorithms.All
        )
        .getOrThrow()

    val signatures = keyRing.generateAttestation(keyRing.addresses)(rawTx.messageToSign).values.toSet
    val thresholdSignature = ThresholdSignatureCurve25519(signatures)
    val attestation = ListMap(addressesToPropMap(sender) -> thresholdSignature)

    rawTx.copy(attestation = attestation)
  }

  def validAssetTransferEd25519Gen(
    keyRing: KeyRing[PrivateKeyEd25519, KeyfileEd25519],
    state:   State,
    fee:     Long = 1L,
    minting: Boolean = false
  ): Gen[AssetTransfer[PublicKeyPropositionEd25519]] = {
    val sender = keyRing.addresses.head
    val asset = AssetValue(1, AssetCode(1: Byte, sender, Latin1Data.unsafe("test")), SecurityRoot.empty)
    val recipients = List((sender, asset))

    val rawTx =
      TransferBuilder
        .buildUnsignedAssetTransfer[PublicKeyPropositionEd25519](
          state,
          AssetTransferRequest(List(sender), recipients, sender, sender, fee, None, minting),
          BoxSelectionAlgorithms.All
        )
        .getOrThrow()

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

  def validAssetTransferGen(
    keyRingCurve25519:        KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    keyRingEd25519:           KeyRing[PrivateKeyEd25519, KeyfileEd25519],
    propsThresholdCurve25519: Set[ThresholdPropositionCurve25519],
    state:                    State,
    fee:                      Long = 1L,
    minting:                  Boolean = false
  ): Gen[AssetTransfer[_ <: Proposition]] =
    Gen.oneOf(
      validAssetTransferCurve25519Gen(keyRingCurve25519, state, fee, minting),
      validAssetTransferThresholdCurve25519Gen(keyRingCurve25519, propsThresholdCurve25519, state, fee, minting),
      validAssetTransferEd25519Gen(keyRingEd25519, state, fee, minting)
    )

  def collectBoxes(addresses: Set[Address], state: State): Seq[TokenBox[TokenValueHolder]] =
    addresses.flatMap(address => state.getTokenBoxes(address)).flatten.toSeq

  def sumBoxes(boxes: Seq[TokenBox[TokenValueHolder]], tokenType: String): Seq[(Address, Int128)] = {
    val boxesByOwner = boxes.groupBy(_.evidence)
    val ownerQuantities = boxesByOwner.map { case (evidence, boxes) =>
      Address(evidence) -> boxes
        .filter(identifier(_).typeString == tokenType)
        .map(_.value.quantity)
        .sum
    }.toSeq
    ownerQuantities.filter(_._2 > 0)
  }
}
