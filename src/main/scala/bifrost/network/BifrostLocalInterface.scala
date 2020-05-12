package bifrost.network

import akka.actor.ActorRef
import bifrost.forging.{Forger, ForgingSettings}
import bifrost.modifier.block.Block
import bifrost.scorexMod.GenericNodeViewHolder
import bifrost.LocalInterface
import bifrost.crypto.PrivateKey25519
import bifrost.modifier.box.proposition.ProofOfKnowledgeProposition
import bifrost.modifier.transaction.bifrostTransaction.BifrostTransaction

class BifrostLocalInterface(override val viewHolderRef: ActorRef, forgerRef: ActorRef, forgingSettings: ForgingSettings)
  extends LocalInterface[ProofOfKnowledgeProposition[PrivateKey25519], BifrostTransaction, Block] {

  import LocalInterface._

  type P = ProofOfKnowledgeProposition[PrivateKey25519]
  type TX = BifrostTransaction
  type PMOD = Block

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

  private def viewHolderEvents: Receive = {
    case stm: GenericNodeViewHolder.StartingPersistentModifierApplication[P, TX, PMOD] =>
      onStartingPersistentModifierApplication(stm.modifier)

    case ft: GenericNodeViewHolder.FailedTransaction[P, TX] =>
      onFailedTransaction(ft.transaction)

    case fm: GenericNodeViewHolder.FailedModification[P, TX, PMOD] =>
      onFailedModification(fm.modifier)

    case st: GenericNodeViewHolder.SuccessfulTransaction[P, TX] =>
      onSuccessfulTransaction(st.transaction)

    case sm: GenericNodeViewHolder.SuccessfulModification[P, TX, PMOD] =>
      onSuccessfulModification(sm.modifier)
  }

  override protected def onStartingPersistentModifierApplication(pmod: Block): Unit = {}

  override protected def onFailedTransaction(tx: BifrostTransaction): Unit = {}

  override protected def onFailedModification(mod: Block): Unit = {}

  override protected def onSuccessfulTransaction(tx: BifrostTransaction): Unit = {}

  override protected def onSuccessfulModification(mod: Block): Unit = {}

  override protected def onNoBetterNeighbour(): Unit = forgerRef ! Forger.StartForging

  override protected def onBetterNeighbourAppeared(): Unit = forgerRef ! Forger.StopForging

  override def receive: Receive = viewHolderEvents orElse {
    case NoBetterNeighbour => onNoBetterNeighbour()
    case BetterNeighbourAppeared => onBetterNeighbourAppeared()
    case lt: LocallyGeneratedTransaction[P, TX] => viewHolderRef ! lt
    case lm: LocallyGeneratedModifier[P, TX, PMOD] => viewHolderRef ! lm
    case a: Any => log.error("Strange input: " + a)
  }
}