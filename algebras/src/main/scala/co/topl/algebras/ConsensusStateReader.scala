package co.topl.algebras

import co.topl.models.utility.Ratio
import co.topl.models._

trait ConsensusStateReader[F[_]] {
  def lookupRelativeStake(epoch: Epoch)(address: TaktikosAddress): F[Option[Ratio]]
  def lookupRegistration(epoch:  Epoch)(address: TaktikosAddress): F[Option[Box.Values.TaktikosRegistration]]
}
