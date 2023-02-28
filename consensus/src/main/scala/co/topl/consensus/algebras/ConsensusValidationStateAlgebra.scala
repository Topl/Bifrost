package co.topl.consensus.algebras

import co.topl.models.utility.Ratio
import co.topl.models._

// TODO: Maybe collapse into a single method returning (Relative Stake, Registration)?
trait ConsensusValidationStateAlgebra[F[_]] {

  /**
   * Determines the "usable" relative stake associated with the given Operator's address at the given "head" Block ID.
   *
   * The interpreter is responsible for returning a result that is derived from data that lags behind `currentBlockId`.
   *
   * @param currentBlockId The ID of the block being validated
   * @param slot The slot to use when considering the epoch to read from.  When validating a header, simply
   *             use the slot of `currentBlockId`.  When producing a header, use the slot being tested.
   * @param address The address of the operator that produced the block
   * @return a ratio, if one exists and is greater than 0.  None otherwise.
   */
  def operatorRelativeStake(currentBlockId: TypedIdentifier, slot: Slot)(
    address: StakingAddresses.Operator
  ): F[Option[Ratio]]

  /**
   * Retrieves the Registration associated with the given operator address
   *
   * The interpreter is responsible for returning a result that is derived from data that lags behind `currentBlockId`.
   *
   * @param currentBlockId The ID of the block being validated
   * @param slot           The slot to use when considering the epoch to read from.  When validating a header, simply
   *                       use the slot of `currentBlockId`.  When producing a header, use the slot being tested.
   * @param address        The address of the operator that produced the block
   * @return a ratio, if one exists and is greater than 0.  None otherwise.
   */
  def operatorRegistration(currentBlockId: TypedIdentifier, slot: Slot)(
    address: StakingAddresses.Operator
  ): F[Option[Box.Values.Registrations.Operator]] // TODO this algebra should return new OperatorNewModel

}
