package co.topl.typeclasses

import cats.data.{EitherT, OptionT}

/**
 * Represents a non-empty sequence of some value `T`.  A PersistentChain maintains a pointer to the first item, the
 * last/latest item, and a cursor to some "current" item.  Values of `T` can be appended to the latest item, although
 * a Failure may be returned if the item is invalid.  The "latest" item can also be removed from the chain to mimic
 * the effect of an undo/rollback operation.
 * @tparam F effectful type constructor (i.e. Future)
 * @tparam T A chainable value `T` (i.e. Block)
 * @tparam Failure The PersistentChain's implementation-specific failure type
 */
trait CursorChain[F[_], T, Failure] {
  type P <: CursorChain[F, T, Failure]

  /**
   * The item to which the cursor currently points
   */
  def current: T

  /**
   * Returns a PersistentChain in which the cursor points to the first item of the chain
   */
  def moveFirst: F[P]

  /**
   * Returns a PersistentChain in which the cursor points to the tip/latest of the chain
   */
  def moveLatest: F[P]

  /**
   * Returns a PersistentChain in which the cursor points to the previous item in the chain.
   * If no previous item exists, None is returned.
   */
  def movePrevious: OptionT[F, P]

  /**
   * Returns a PersistentChain in which the cursor points to the next item in the chain.
   * If no next item exists, None is returned.
   */
  def moveNext: OptionT[F, P]

  /**
   * Points the cursor to the latest item and attempts to append the given item T to it.
   * If non-applicable or invalid, a Failure is returned
   */
  def appendToLatest(t: T): EitherT[F, Failure, P]

  /**
   * Points the cursor to the latest item and attempts to remove it.
   * If currently at `first`, None is instead returned. Otherwise, (the new chain, the removed value) is returned.
   */
  def popLatest(): OptionT[F, (P, T)]
}

/**
 * A `PersistentChain` that maintains some state T at the tip of the chain.  Appending or popping blocks should
 * update this state accordingly.
 *
 * @tparam F effectful type constructor (i.e. Future)
 * @tparam T A chainable value `T` (i.e. Block)
 * @tparam State A stateful representation of the tip of the chain (i.e. box registry)
 * @tparam Failure The PersistentChain's implementation-specific failure type
 */
trait StatefulCursorChain[F[_], T, State, Failure] extends CursorChain[F, T, Failure] {
  override type P <: StatefulCursorChain[F, T, State, Failure]

  /**
   * Fetches the state associated with the tip of the chain
   */
  def latestState(): EitherT[F, Failure, State]
}
