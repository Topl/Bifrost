package examples.bifrost

import akka.actor.ActorRef
import examples.bifrost.blocks.BifrostBlock
import examples.bifrost.forging.{Forger, ForgingSettings}
import examples.bifrost.transaction.BifrostTransaction
import scorex.core.LocalInterface
import scorex.core.transaction.box.proposition.ProofOfKnowledgeProposition
import scorex.core.transaction.state.PrivateKey25519

class BifrostLocalInterface(override val viewHolderRef: ActorRef, forgerRef: ActorRef, forgingSettings: ForgingSettings)
  extends LocalInterface[ProofOfKnowledgeProposition[PrivateKey25519], BifrostTransaction, BifrostBlock] {

  override protected def onStartingPersistentModifierApplication(pmod: BifrostBlock): Unit = {}

  override protected def onFailedTransaction(tx: BifrostTransaction): Unit = {}

  override protected def onFailedModification(mod: BifrostBlock): Unit = {}

  override protected def onSuccessfulTransaction(tx: BifrostTransaction): Unit = {}

  override protected def onSuccessfulModification(mod: BifrostBlock): Unit = {}

  override protected def onNoBetterNeighbour(): Unit = forgerRef ! Forger.StartMining

  override protected def onBetterNeighbourAppeared(): Unit = forgerRef ! Forger.StopMining
}