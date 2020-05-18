package bifrost.network

import akka.actor.{Actor, ActorRef}
import bifrost.forging.{Forger, ForgingSettings}
import bifrost.modifier.block.Block
import bifrost.scorexMod.GenericNodeViewHolder
import bifrost.crypto.PrivateKey25519
import bifrost.modifier.box.proposition.{ProofOfKnowledgeProposition, Proposition}
import bifrost.modifier.transaction.bifrostTransaction.{BifrostTransaction, Transaction}
import bifrost.nodeView.PersistentNodeViewModifier
import bifrost.utils.Logging

class BifrostLocalInterface(viewHolderRef: ActorRef, forgerRef: ActorRef, forgingSettings: ForgingSettings)
  extends Actor with Logging {

  import BifrostLocalInterface._

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

  protected def onStartingPersistentModifierApplication(pmod: Block): Unit = {}

  protected def onFailedTransaction(tx: BifrostTransaction): Unit = {}

  protected def onFailedModification(mod: Block): Unit = {}

  protected def onSuccessfulTransaction(tx: BifrostTransaction): Unit = {}

  protected def onSuccessfulModification(mod: Block): Unit = {}

  protected def onNoBetterNeighbour(): Unit = forgerRef ! Forger.StartForging

  protected def onBetterNeighbourAppeared(): Unit = forgerRef ! Forger.StopForging

  override def receive: Receive = viewHolderEvents orElse {
    case NoBetterNeighbour => onNoBetterNeighbour()
    case BetterNeighbourAppeared => onBetterNeighbourAppeared()
    case lt: LocallyGeneratedTransaction[P, TX] => viewHolderRef ! lt
    case lm: LocallyGeneratedModifier[P, TX, PMOD] => viewHolderRef ! lm
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
