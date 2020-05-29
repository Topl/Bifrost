package bifrost.old_network.message

import akka.actor.{Actor, ActorRef}
import bifrost.crypto.PrivateKey25519
import bifrost.forging.{Forger, ForgingSettings}
import bifrost.modifier.block.Block
import bifrost.modifier.box.proposition.{ProofOfKnowledgeProposition, Proposition}
import bifrost.modifier.transaction.bifrostTransaction.{GenericTransaction, Transaction}
import bifrost.nodeView.{GenericNodeViewHolder, PersistentNodeViewModifier}
import bifrost.utils.Logging

class BifrostLocalInterface(viewHolderRef: ActorRef, forgerRef: ActorRef, forgingSettings: ForgingSettings)
  extends Actor with Logging {

  import BifrostLocalInterface._

  type P = ProofOfKnowledgeProposition[PrivateKey25519]
  type TX = Transaction
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
    case stm: GenericNodeViewHolder.StartingPersistentModifierApplication[P, TX, PMOD] @unchecked =>
      onStartingPersistentModifierApplication(stm.modifier)

    case ft: GenericNodeViewHolder.FailedTransaction[P, TX] @unchecked =>
      onFailedTransaction(ft.transaction)

    case fm: GenericNodeViewHolder.FailedModification[P, TX, PMOD] @unchecked =>
      onFailedModification(fm.modifier)

    case st: GenericNodeViewHolder.SuccessfulTransaction[P, TX] @unchecked =>
      onSuccessfulTransaction(st.transaction)

    case sm: GenericNodeViewHolder.SuccessfulModification[P, TX, PMOD] @unchecked =>
      onSuccessfulModification(sm.modifier)
  }

  protected def onStartingPersistentModifierApplication(pmod: Block): Unit = {}

  protected def onFailedTransaction(tx: Transaction): Unit = {}

  protected def onFailedModification(mod: Block): Unit = {}

  protected def onSuccessfulTransaction(tx: Transaction): Unit = {}

  protected def onSuccessfulModification(mod: Block): Unit = {}

  protected def onNoBetterNeighbour(): Unit = forgerRef ! Forger.StartForging

  protected def onBetterNeighbourAppeared(): Unit = forgerRef ! Forger.StopForging

  override def receive: Receive = viewHolderEvents orElse {
    case NoBetterNeighbour => onNoBetterNeighbour()
    case BetterNeighbourAppeared => onBetterNeighbourAppeared()
    case lt: LocallyGeneratedTransaction[P, TX] @unchecked => viewHolderRef ! lt
    case lm: LocallyGeneratedModifier[P, TX, PMOD] @unchecked => viewHolderRef ! lm
    case a: Any => log.error("Strange input: " + a)
  }
}

object BifrostLocalInterface {

  case object NoBetterNeighbour

  case object BetterNeighbourAppeared

  case class LocallyGeneratedTransaction[P <: Proposition, TX <: GenericTransaction[P]](tx: TX)

  case class LocallyGeneratedModifier[P <: Proposition, TX <: GenericTransaction[P], PMOD <: PersistentNodeViewModifier[P, TX]]
  (pmod: PMOD)
}
