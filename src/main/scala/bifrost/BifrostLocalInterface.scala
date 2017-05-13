package bifrost

import akka.actor.ActorRef
import bifrost.blocks.BifrostBlock
import bifrost.forging.{Forger, ForgingSettings}
import bifrost.scorexMod.GenericNodeViewHolder
import bifrost.transaction.BifrostTransaction
import scorex.core.{LocalInterface, PersistentNodeViewModifier}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition}
import scorex.core.transaction.state.PrivateKey25519

class BifrostLocalInterface(override val viewHolderRef: ActorRef, forgerRef: ActorRef, forgingSettings: ForgingSettings)
  extends LocalInterface[ProofOfKnowledgeProposition[PrivateKey25519], BifrostTransaction, BifrostBlock] {

  import BifrostLocalInterface._

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

  override protected def onStartingPersistentModifierApplication(pmod: BifrostBlock): Unit = {}

  override protected def onFailedTransaction(tx: BifrostTransaction): Unit = {}

  override protected def onFailedModification(mod: BifrostBlock): Unit = {}

  override protected def onSuccessfulTransaction(tx: BifrostTransaction): Unit = {}

  override protected def onSuccessfulModification(mod: BifrostBlock): Unit = {}

  override protected def onNoBetterNeighbour(): Unit = forgerRef ! Forger.StartForging

  override protected def onBetterNeighbourAppeared(): Unit = forgerRef ! Forger.StopForging

  override def receive: Receive = viewHolderEvents orElse {
    case NoBetterNeighbour =>
      onNoBetterNeighbour()
    case BetterNeighbourAppeared =>
      onBetterNeighbourAppeared()
    case lt: LocallyGeneratedTransaction[P, TX] =>
      viewHolderRef ! lt
    case lm: LocallyGeneratedModifier[P, TX, PMOD] =>
      viewHolderRef ! lm
    case a: Any => log.error("Strange input: " + a)
  }
}

object BifrostLocalInterface {

  case object NoBetterNeighbour

  case object BetterNeighbourAppeared

  case class LocallyGeneratedTransaction[P <: Proposition, TX <: Transaction[P]](tx: TX)

  case class LocallyGeneratedModifier[P <: Proposition, TX <: Transaction[P], PMOD <: PersistentNodeViewModifier[P, TX]]
  (pmod: PMOD)

}
