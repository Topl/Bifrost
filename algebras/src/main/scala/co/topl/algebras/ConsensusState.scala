package co.topl.algebras

import co.topl.models.utility.Ratio
import co.topl.models._

trait ConsensusStateReader[F[_]] {
  def lookupRelativeStake(epoch: Epoch)(address: TaktikosAddress): F[Option[Ratio]]
  def lookupRegistration(epoch:  Epoch)(address: TaktikosAddress): F[Option[Box.Values.TaktikosRegistration]]
}

trait ConsensusState[F[_]] extends ConsensusStateReader[F] {
  def writeRelativeStakes(epoch: Epoch, relativeStakes: Map[TaktikosAddress, Ratio]): F[Unit]

  def foldRelativeStakes[S](epoch: Epoch)(s: S)(f: (S, (TaktikosAddress, Ratio)) => F[S]): F[S]

  def writeRegistrations(epoch: Epoch, registrations: Map[TaktikosAddress, Box.Values.TaktikosRegistration]): F[Unit]

  def foldRegistrations[S](epoch: Epoch)(s: S)(f: (S, (TaktikosAddress, Box.Values.TaktikosRegistration)) => F[S]): F[S]

}
