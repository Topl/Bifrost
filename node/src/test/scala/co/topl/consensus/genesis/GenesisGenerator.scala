package co.topl.consensus.genesis

import co.topl.attestation.EvidenceProducer.Syntax.ProducerOps
import co.topl.attestation._
import co.topl.attestation.keyManagement._
import co.topl.consensus.NxtConsensus
import co.topl.crypto.{PrivateKey, PublicKey}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.box.{ArbitBox, SimpleValue}
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer}
import co.topl.settings.GenesisGenerationSettings
import co.topl.utils.Int128
import co.topl.utils.NetworkType.NetworkPrefix

import scala.collection.SortedSet
import scala.collection.immutable.ListMap

case class GenesisGenerator(
  keyRingCurve25519:               KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
  keyRingEd25519:                  KeyRing[PrivateKeyEd25519, KeyfileEd25519],
  propositionsThresholdCurve25519: Set[ThresholdPropositionCurve25519]
)(implicit networkPrefix:          NetworkPrefix) {

  def GenesisGen: GenesisProvider[GenesisGenerationSettings] = strategy => {
    val (_, balance, initialDifficulty) =
      (strategy.numberOfParticipants, strategy.balanceForEachParticipant, strategy.initialDifficulty)

    val (addressesCurve25519, addressesEd25519, addressesThresholdCurve25519) =
      (keyRingCurve25519.addresses, keyRingEd25519.addresses, propositionsThresholdCurve25519.map(_.address))

    val genesisAcctCurve25519: PrivateKeyCurve25519 =
      new PrivateKeyCurve25519(PrivateKey(Array.fill(32)(2: Byte)), PublicKey(Array.fill(32)(2: Byte)))

    val genesisAcctEd25519: PrivateKeyEd25519 =
      new PrivateKeyEd25519(PrivateKey(Array.fill(32)(2: Byte)), PublicKey(Array.fill(32)(2: Byte)))

    val genesisThresholdPublicKey = ThresholdPropositionCurve25519(
      0,
      SortedSet[PublicKeyPropositionCurve25519](genesisAcctCurve25519.publicImage)
    )

    // map the members to their balances then continue as normal
    val totalStake =
      (addressesCurve25519.size + addressesEd25519.size + addressesThresholdCurve25519.size) * balance

    val txInputCurve25519 = (
      IndexedSeq(),
      (genesisAcctCurve25519.publicImage.address -> SimpleValue(0L)) +: addressesCurve25519
        .map(_ -> SimpleValue(balance))
        .toIndexedSeq,
      ListMap(genesisAcctCurve25519.publicImage -> SignatureCurve25519.genesis),
      Int128(0),
      0L,
      None,
      true
    )

    val txInputThresholdCurve25519 = (
      IndexedSeq(),
      (genesisThresholdPublicKey.address -> SimpleValue(0L)) +: addressesThresholdCurve25519
        .map(_ -> SimpleValue(balance))
        .toIndexedSeq,
      ListMap(genesisThresholdPublicKey -> ThresholdSignatureCurve25519.genesis),
      Int128(0),
      0L,
      None,
      true
    )

    val txInputEd25519 = (
      IndexedSeq(),
      (genesisAcctEd25519.publicImage.address -> SimpleValue(0L)) +: addressesEd25519
        .map(_ -> SimpleValue(balance))
        .toIndexedSeq,
      ListMap(genesisAcctEd25519.publicImage -> SignatureEd25519.genesis),
      Int128(0),
      0L,
      None,
      true
    )

    val txs = Seq(
      (ArbitTransfer[PublicKeyPropositionCurve25519] _).tupled(txInputCurve25519),
      (PolyTransfer[PublicKeyPropositionCurve25519] _).tupled(txInputCurve25519),
      (ArbitTransfer[ThresholdPropositionCurve25519] _).tupled(txInputThresholdCurve25519),
      (PolyTransfer[ThresholdPropositionCurve25519] _).tupled(txInputThresholdCurve25519),
      (ArbitTransfer[PublicKeyPropositionEd25519] _).tupled(txInputEd25519),
      (PolyTransfer[PublicKeyPropositionEd25519] _).tupled(txInputEd25519)
    )

    val generatorBox = ArbitBox(genesisAcctCurve25519.publicImage.generateEvidence, 0, SimpleValue(totalStake))
    val signature = SignatureCurve25519.genesis
    val blockVersion = strategy.genesisApplicationVersion.blockByte

    val block =
      Block(
        ModifierId.genesisParentId,
        0L,
        generatorBox,
        genesisAcctCurve25519.publicImage,
        signature,
        1L,
        initialDifficulty,
        txs,
        blockVersion
      )

    val state = NxtConsensus.State(Int128(totalStake), strategy.initialDifficulty, 0L, 0L)
    NxtConsensus.Genesis(block, state)
  }
}
