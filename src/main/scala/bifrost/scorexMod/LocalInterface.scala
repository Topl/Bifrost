package bifrost

import akka.actor.{Actor, ActorRef}
import bifrost.LocalInterface.{BetterNeighbourAppeared, LocallyGeneratedModifier, LocallyGeneratedTransaction, NoBetterNeighbour}
// import bifrost.NodeViewHolder._
import bifrost.scorexMod.GenericNodeViewHolder
import bifrost.scorexMod.GenericNodeViewHolder._
import bifrost.transaction.Transaction
import bifrost.transaction.box.proposition.Proposition
import bifrost.utils.ScorexLogging

/**
  *
  */
trait LocalInterface[P <: Proposition, TX <: Transaction[P], PMOD <: PersistentNodeViewModifier[P, TX]]
  extends Actor with ScorexLogging {

  val viewHolderRef: ActorRef

  override def preStart(): Unit = {
    val events = Seq(
      GenericNodeViewHolder.EventType.StartingPersistentModifierApplication,

      GenericNodeViewHolder.EventType.FailedTransaction,
      GenericNodeViewHolder.EventType.FailedPersistentModifier,
      GenericNodeViewHolder.EventType.SuccessfulTransaction,
      GenericNodeViewHolder.EventType.SuccessfulPersistentModifier
    )
    viewHolderRef ! Subscribe(events)
  }

  private def viewHolderEvents: Receive = {
    case stm: StartingPersistentModifierApplication[P, TX, PMOD] =>
      onStartingPersistentModifierApplication(stm.modifier)

    case ft: FailedTransaction[P, TX] =>
      onFailedTransaction(ft.transaction)

    case fm: FailedModification[P, TX, PMOD] =>
      onFailedModification(fm.modifier)

    case st: SuccessfulTransaction[P, TX] =>
      onSuccessfulTransaction(st.transaction)

    case sm: SuccessfulModification[P, TX, PMOD] =>
      onSuccessfulModification(sm.modifier)
  }

  protected def onStartingPersistentModifierApplication(pmod: PMOD): Unit

  protected def onFailedTransaction(tx: TX): Unit

  protected def onFailedModification(mod: PMOD): Unit

  protected def onSuccessfulTransaction(tx: TX): Unit

  protected def onSuccessfulModification(mod: PMOD): Unit

  protected def onNoBetterNeighbour(): Unit

  protected def onBetterNeighbourAppeared(): Unit

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

object LocalInterface {

  case object NoBetterNeighbour

  case object BetterNeighbourAppeared

  case class LocallyGeneratedTransaction[P <: Proposition, TX <: Transaction[P]](tx: TX)

  case class LocallyGeneratedModifier[P <: Proposition, TX <: Transaction[P], PMOD <: PersistentNodeViewModifier[P, TX]]
  (pmod: PMOD)

}