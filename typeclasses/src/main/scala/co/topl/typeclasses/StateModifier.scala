package co.topl.typeclasses

import cats.implicits._
import cats.kernel.Semigroup
import co.topl.models._
import simulacrum.{op, typeclass}

@typeclass trait StateModifier[T] {

  @op("stateModifications") def stateModificationsOf(t: T): StateModifications

}

case class StateModifications(opensBoxIds: Set[TypedIdentifier], createsBoxes: Set[Box])

object StateModifications {

  implicit val semigroup: Semigroup[StateModifications] =
    (a, b) => StateModifications(a.opensBoxIds ++ b.opensBoxIds, a.createsBoxes ++ b.createsBoxes)
}

object StateModifierInstances {
  import StateModifications._
  import StateModifier.ops._

  implicit val transactionStateModifier: StateModifier[Transaction] =
    tx => ???

  implicit val blockStateModifier: StateModifier[Block] = { case b: BlockV1 =>
    b.transactions.map(_.stateModifications).fold(StateModifications(Set.empty, Set.empty))(_.combine(_))
  }
}
