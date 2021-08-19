package co.topl.ledger

import cats.data.{EitherT, OptionT}
import co.topl.models.{Address, BlockBodyV2, Box, Transaction, TypedIdentifier}
import co.topl.typeclasses.StatefulCursorChain

/**
 * A chain of Block Bodies with a stateful representation meant for a Blockchain "Ledger"
 *
 * @tparam F effectful type constructor (i.e. Future) for ledger operations
 * @tparam StateF effectful type constructor (i.e. Future) for state operations
 * @tparam StateFailure Type of failure for state operations
 */
trait LedgerChain[F[_], StateF[_], StateFailure]
    extends StatefulCursorChain[F, BlockBodyV2, LedgerChain.State[StateF, StateFailure], LedgerChain.Failure] {
  override type P <: LedgerChain[F, StateF, StateFailure]
}

object LedgerChain {

  sealed abstract class Failure

  trait State[F[_], Failure] {
    type S <: State[F, Failure]
    def box(id:                  TypedIdentifier): OptionT[F, Box]
    def boxesForAddress(address: Address): EitherT[F, Failure, Seq[Box]]
    def transaction(id:          TypedIdentifier): OptionT[F, Transaction]
    def apply(value:             BlockBodyV2): EitherT[F, Failure, S]
    def unapply(): EitherT[F, Failure, S]
  }
}
