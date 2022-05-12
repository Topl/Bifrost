package co.topl.algebras

import co.topl.models.utility.Ratio
import co.topl.models._

trait ConsensusStateReader[F[_]] {
  def lookupRelativeStake(epoch: Epoch)(address: StakingAddress): F[Option[Ratio]]
  def lookupRegistration(epoch:  Epoch)(address: StakingAddress): F[Option[Box.Values.Registrations.Pool]]
}
