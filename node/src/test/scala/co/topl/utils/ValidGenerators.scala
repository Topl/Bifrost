package co.topl.utils

import co.topl.attestation.PublicKeyPropositionCurve25519.evProducer
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.consensus.genesis.PrivateGenesis
import co.topl.keyManagement.{KeyRing, KeyfileCurve25519, PrivateKeyCurve25519}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.box.Box.identifier
import co.topl.modifier.box.{AssetCode, AssetValue, SecurityRoot, SimpleValue, _}
import co.topl.modifier.transaction.Transaction.TX
import co.topl.modifier.transaction._
import co.topl.nodeView.history.History
import co.topl.nodeView.state.State
import co.topl.settings.AppSettings
import org.scalacheck.Gen

import scala.util.{Failure, Random, Success}

trait ValidGenerators extends CoreGenerators {

  val keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] =
    KeyRing.empty(settings.application.keyFileDir)

  val genesisBlock: Block = PrivateGenesis(
    keyRing.generateNewKeyPairs(num = 3) match {
      case Success(keys) => keys.map(_.publicImage.address)
      case Failure(ex)   => throw ex
    },
    settings
  ).getGenesisBlock.get._1

  val genesisBlockId: ModifierId = genesisBlock.id

  val genesisState: State = genesisState(settings)

  lazy val validBifrostTransactionSeqGen: Gen[Seq[TX]] = for {
    seqLen <- positiveMediumIntGen
  } yield 0 until seqLen map { _ =>
    val g: Gen[TX] = sampleUntilNonEmpty(Gen.oneOf(transactionTypes))
    sampleUntilNonEmpty(g)
  }

  lazy val validPolyTransferGen: Gen[PolyTransfer[_]] = for {
    from        <- fromSeqGen
    to          <- toSeqGen
    attestation <- attestationGen
    key         <- publicKeyPropositionCurve25519Gen
    fee         <- positiveLongGen
    timestamp   <- positiveLongGen
    data        <- stringGen
  } yield {

    val tx = PolyTransfer(from, to, attestation, fee, timestamp, Some(data), minting = false)
    val sig = key._1.sign(tx.messageToSign)
    tx.copy(attestation = Map(key._2 -> sig))
  }

  lazy val validArbitTransferGen: Gen[ArbitTransfer[_]] = for {
    from        <- fromSeqGen
    to          <- toSeqGen
    attestation <- attestationGen
    fee         <- positiveLongGen
    timestamp   <- positiveLongGen
    data        <- stringGen
  } yield ArbitTransfer(from, to, attestation, fee, timestamp, Some(data), minting = false)

  lazy val validAssetTransferGen: Gen[AssetTransfer[_]] = for {
    from        <- fromSeqGen
    to          <- assetToSeqGen
    attestation <- attestationGen
    fee         <- positiveLongGen
    timestamp   <- positiveLongGen
    data        <- stringGen
  } yield AssetTransfer(from, to, attestation, fee, timestamp, Some(data), minting = true)

  def genesisState(settings: AppSettings, genesisBlockWithVersion: Block = genesisBlock): State = {
    History.readOrGenerate(settings).append(genesisBlock)
    State.genesisState(settings, Seq(genesisBlockWithVersion))
  }

  def validPolyTransfer(
    keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    state:   State,
    fee:     Long = 1L
  ): Gen[PolyTransfer[PublicKeyPropositionCurve25519]] = {

    val availablePolys = sumBoxes(collectBoxes(keyRing.addresses, state), "PolyBox")
    val (sender, poly) = availablePolys(Random.nextInt(availablePolys.length))
    val polyAmount = SimpleValue(Int128(sampleUntilNonEmpty(Gen.chooseNum(1L + fee, poly.longValue() - 1))) - fee)

    val recipients = {
      val address: Address = keyRing.addresses.filterNot(_ == sender).toSeq(Random.nextInt(keyRing.addresses.size - 1))
      IndexedSeq((address, polyAmount))
    }
    val rawTx = PolyTransfer
      .createRaw(
        state,
        recipients,
        IndexedSeq(sender),
        changeAddress = sender,
        fee,
        data = None
      )
      .get

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

  def validArbitTransfer(
    keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    state:   State,
    fee:     Long = 1L
  ): Gen[ArbitTransfer[PublicKeyPropositionCurve25519]] = {

    val availableArbits = sumBoxes(collectBoxes(keyRing.addresses, state), "ArbitBox")
    val (sender, arbit) = availableArbits(Random.nextInt(availableArbits.length))
    val arbitAmount = SimpleValue(Int128(sampleUntilNonEmpty(Gen.chooseNum(1L + fee, arbit.longValue() - 1))) - fee)

    val recipients = {
      val address = keyRing.addresses.filterNot(_ == sender).toSeq(Random.nextInt(keyRing.addresses.size - 1))
      IndexedSeq((address, arbitAmount))
    }
    val rawTx = ArbitTransfer
      .createRaw(
        state,
        recipients,
        IndexedSeq(sender),
        changeAddress = sender,
        consolidationAddress = sender,
        fee,
        data = None
      )
      .get

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

  def validAssetTransfer(
    keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    state:   State,
    fee:     Long = 1L,
    minting: Boolean = false
  ): Gen[AssetTransfer[PublicKeyPropositionCurve25519]] = {
    val sender = keyRing.addresses.head
    val asset = AssetValue(1, AssetCode(1: Byte, sender, "test"), SecurityRoot.empty)
    val recipients = IndexedSeq((sender, asset))

    // todo: This should not be using the create raw function because we are testing too many things then!
    val rawTx = AssetTransfer
      .createRaw(
        state,
        recipients,
        IndexedSeq(sender),
        changeAddress = sender,
        consolidationAddress = sender,
        fee,
        data = None,
        minting
      )
      .get

    rawTx.copy(attestation = Transaction.updateAttestation(rawTx)(keyRing.generateAttestation(sender)))
  }

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
