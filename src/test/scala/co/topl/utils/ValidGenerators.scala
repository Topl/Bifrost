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

  val genesisState: State = genesisState(settings)

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

  def genesisState(settings: AppSettings, genesisBlockWithVersion: Block = genesisBlock): State = {
    History.readOrGenerate(settings).append(genesisBlock)
    State.genesisState(settings, Seq(genesisBlockWithVersion))
  }

  def validPolyTransfer(
                         keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
                         state: State,
                         fee: Long = 1L
                       ): Gen[PolyTransfer[PublicKeyPropositionCurve25519]] = {

    val availablePolys = sumBoxes(collectBoxes(keyRing.addresses, state), "PolyBox")
    val (sender, poly) = availablePolys(Random.nextInt(availablePolys.length))
    val polyAmount = SimpleValue(Int128(sampleUntilNonEmpty(Gen.chooseNum(1L, poly.longValue()))))

    val prop = keyRing.lookupPublicKey(sender).get
    val recipients = IndexedSeq((keyRing.addresses.toSeq(Random.nextInt(keyRing.addresses.size)), polyAmount))
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

    val availableArbits = sumBoxes(collectBoxes(keyRing.addresses, state), "ArbitBox")
    val (sender, arbit) = availableArbits(Random.nextInt(availableArbits.length))
    val arbitAmount = SimpleValue(Int128(sampleUntilNonEmpty(Gen.chooseNum(1L, arbit.longValue()))))

    val prop = keyRing.lookupPublicKey(sender).get
    val recipients = IndexedSeq((keyRing.addresses.toSeq(Random.nextInt(keyRing.addresses.size)), arbitAmount))
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
      val availableAssets = sumBoxes(collectBoxes(keyRing.addresses, state), "AssetBox")
      val sender = availableAssets(Random.nextInt(availableAssets.length))
      val assetAmount = Gen.chooseNum(1L, sender._2.longValue()).sample.get
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

  def sumBoxes(boxes: Seq[TokenBox[TokenValueHolder]], tokenType: String): Seq[(Address, Int128)] = {
    val boxesByOwner = boxes.groupBy(_.evidence)
    val ownerQuantities = boxesByOwner.map {
      case (evidence, boxes) =>
        Address(evidence) -> boxes
          .filter(identifier(_).typeString == tokenType).map(_.value.quantity).foldLeft[Int128](0)(_ + _)
    }.toSeq
    ownerQuantities.filter(_._2 > 0)
  }
}

