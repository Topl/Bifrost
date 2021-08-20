package co.topl.typeclasses

import cats.implicits._
import cats.kernel.Semigroup
import co.topl.models._
import co.topl.typeclasses.ContainsTransactions.ops._
import simulacrum.{op, typeclass}

/**
 * Satisfies that T can modify the state of Boxes
 */
@typeclass trait ModifiesBoxState[T] {

  @op("stateModifications") def stateModificationsOf(t: T): StateModifications

}

object ModifiesBoxState {

  object Instances {

    import ModifiesBoxState.ops._
    import StateModifications._

    implicit val transactionStateModifier: ModifiesBoxState[Transaction] =
      tx => ???

    implicit def containsTransactionsModifier[T: ContainsTransactions]: ModifiesBoxState[T] =
      _.transactions.map(_.stateModifications).fold(StateModifications(Set.empty, Set.empty))(_.combine(_))
  }
}

case class StateModifications(opensBoxIds: Set[TypedIdentifier], createsBoxes: Set[Box])

object StateModifications {

  implicit val semigroup: Semigroup[StateModifications] =
    (a, b) => StateModifications(a.opensBoxIds ++ b.opensBoxIds, a.createsBoxes ++ b.createsBoxes)
}
