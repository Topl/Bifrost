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
   * @param address The address of the operator that produced the block
   * @return a ratio, if one exists and is greater than 0.  None otherwise.
   */
  def operatorRelativeStake(currentBlockId: TypedIdentifier)(address: StakingAddresses.Operator): F[Option[Ratio]]

  /**
   * Retrieves the Registration associated with the given operator address
   *
   * The interpreter is responsible for returning a result that is derived from data that lags behind `currentBlockId`.
   *
   * @param currentBlockId The ID of the block being validated
   * @param address The address of the operator that produced the block
   * @return a ratio, if one exists and is greater than 0.  None otherwise.
   */
  def operatorRegistration(currentBlockId: TypedIdentifier)(
    address:                               StakingAddresses.Operator
  ): F[Option[Box.Values.Registrations.Operator]]

}
