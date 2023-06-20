package co.topl.consensus.algebras

import cats.Monad
import cats.data.OptionT
import cats.implicits._
import co.topl.consensus.models.{ActiveStaker, BlockId, StakingAddress}
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.numerics.implicits._

trait ConsensusValidationStateAlgebra[F[_]] {

  /**
   * Determines the "usable" total active stake associated.
   *
   * The interpreter is responsible for returning a result that is derived from data that lags behind `currentBlockId`.
   *
   * @param currentBlockId The ID of the block being validated
   * @param slot The slot to use when considering the epoch to read from.  When validating a header, simply
   *             use the slot of `currentBlockId`.  When producing a header, use the slot being tested.
   * @return a value indicating the total usable stake
   */
  def totalActiveStake(currentBlockId: BlockId, slot: Slot): F[BigInt]

  /**
   * Retrieves the ActiveStaker associated with the given operator address
   *
   * The interpreter is responsible for returning a result that is derived from data that lags behind `currentBlockId`.
   *
   * @param currentBlockId The ID of the block being validated
   * @param slot           The slot to use when considering the epoch to read from.  When validating a header, simply
   *                       use the slot of `currentBlockId`.  When producing a header, use the slot being tested.
   * @param address        The address of the operator that produced the block
   * @return an ActiveStaker, if one exists.  None otherwise.
   */
  def staker(currentBlockId: BlockId, slot: Slot)(address: StakingAddress): F[Option[ActiveStaker]]

  /**
   * Determines the "usable" relative stake associated with the given Operator's address at the given "head" Block ID.
   *
   * @param currentBlockId The ID of the block being validated
   * @param slot           The slot to use when considering the epoch to read from.  When validating a header, simply
   *                       use the slot of `currentBlockId`.  When producing a header, use the slot being tested.
   * @param address        The address of the operator that produced the block
   * @return a ratio, if one exists and is greater than 0.  None otherwise.
   */
  def operatorRelativeStake(currentBlockId: BlockId, slot: Slot)(
    address: StakingAddress
  )(implicit monadF: Monad[F]): F[Option[Ratio]] =
    OptionT(staker(currentBlockId, slot)(address))
      .semiflatMap(registration => totalActiveStake(currentBlockId, slot).map(Ratio(registration.quantity: BigInt, _)))
      .value

}
