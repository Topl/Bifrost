/*
package examples.bifrost

import examples.bifrost.blocks.{BifrostBlock, BifrostBlockCompanion}
import examples.bifrost.state.BifrostState
import examples.bifrost.transaction.{BifrostTransaction, StableCoinTransfer}
import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.{NodeViewHolder, NodeViewModifier}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.util.{Failure, Success}

class SimpleNodeViewHolder(settings: Settings)
  extends NodeViewHolder[PublicKey25519Proposition, BifrostTransaction, BifrostBlock] {


  override val networkChunkSize: Int = settings.networkChunkSize
  override type SI = SimpleSyncInfo

  override type HIS = BifrostBlockchain
  override type MS = BifrostState
  override type VL = BifrostUI
  override type MP = BifrostMemPool

  override lazy val modifierCompanions: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] =
    Map(BifrostBlock.ModifierTypeId -> BifrostBlockCompanion)

  override def restoreState(): Option[(HIS, MS, VL, MP)] = None

  override protected def genesisState: (HIS, MS, VL, MP) = {
    val emptyState = new BifrostState()

    val genesisAcc1 = BifrostUI(Base58.decode("genesisoo").get).publicKeys.head
    val genesisAcc2 = BifrostUI(Base58.decode("genesiso1").get).publicKeys.head

    val IntitialBaseTarget = 15372286700L
    val generator = PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte))
    val toInclude: Seq[BifrostTransaction] = Seq(
      StableCoinTransfer(genesisAcc1, genesisAcc1, 50000000, 0),
      StableCoinTransfer(genesisAcc2, genesisAcc2, 50000000, 0)
    )

    val genesisBlock: BifrostBlock = BifrostBlock(Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
      0L, Array.fill(BifrostBlock.SignatureLength)(0: Byte), IntitialBaseTarget, generator, toInclude)

    val history = BifrostHistory.append(genesisBlock) match {
      case Failure(f) => throw f
      case Success(newBlockchain) => newBlockchain._1
    }
    require(history.height() == 1, s"${history.height()} == 1")

    val state = emptyState.applyChanges(emptyState.changes(genesisBlock).get, genesisBlock.id) match {
      case Failure(f) => throw f
      case Success(newState) => newState
    }
    require(!state.isEmpty)

    log.info(s"Genesis state with block (id: ${genesisBlock.id}) ${genesisBlock.json.noSpaces} created")

    (history, state, SimpleWallet(settings.walletSeed), new SimpleMemPool)
  }
}
*/
