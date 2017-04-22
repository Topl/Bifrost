package examples.bifrost

import akka.actor.ActorRef
import examples.bifrost.blocks.BifrostBlock
import examples.bifrost.forging.{Forger, ForgingSettings}
import examples.bifrost.scorexMod.GenericNodeViewHolder
import examples.bifrost.transaction.BifrostTransaction
import scorex.core.{LocalInterface, PersistentNodeViewModifier}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition}
import scorex.core.transaction.state.PrivateKey25519

class BifrostLocalInterface(override val viewHolderRef: ActorRef, forgerRef: ActorRef, forgingSettings: ForgingSettings)
  extends LocalInterface[ProofOfKnowledgeProposition[PrivateKey25519], BifrostTransaction, BifrostBlock] {

  type P = ProofOfKnowledgeProposition[PrivateKey25519]
  type TX = BifrostTransaction
  type PMOD = BifrostBlock

  override def preStart(): Unit = {
    val events = Seq(
      GenericNodeViewHolder.EventType.StartingPersistentModifierApplication,

      GenericNodeViewHolder.EventType.FailedTransaction,
      GenericNodeViewHolder.EventType.FailedPersistentModifier,
      GenericNodeViewHolder.EventType.SuccessfulTransaction,
      GenericNodeViewHolder.EventType.SuccessfulPersistentModifier
    )
    viewHolderRef ! GenericNodeViewHolder.Subscribe(events)
  }

  override protected def onStartingPersistentModifierApplication(pmod: BifrostBlock): Unit = {}

  override protected def onFailedTransaction(tx: BifrostTransaction): Unit = {}

  override protected def onFailedModification(mod: BifrostBlock): Unit = {}

  override protected def onSuccessfulTransaction(tx: BifrostTransaction): Unit = {}

  override protected def onSuccessfulModification(mod: BifrostBlock): Unit = {}

  override protected def onNoBetterNeighbour(): Unit = forgerRef ! Forger.StartForging

  override protected def onBetterNeighbourAppeared(): Unit = forgerRef ! Forger.StopForging

}

