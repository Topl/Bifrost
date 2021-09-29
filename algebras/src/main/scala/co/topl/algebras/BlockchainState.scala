package co.topl.algebras

import cats.tagless.autoFunctorK
import co.topl.models.{BlockV2, Box, Epoch, Eta, TaktikosAddress, TypedIdentifier}
import co.topl.models.utility.Ratio

@autoFunctorK
trait BlockchainState[F[_]] {
  def genesis: F[BlockV2]
  def canonicalHead: F[BlockV2]
  def append(blockV2:            BlockV2): F[Unit]
  def lookupBlock(id:            TypedIdentifier): F[Option[BlockV2]]
  def lookupRelativeStake(epoch: Epoch)(address:        TaktikosAddress): F[Option[Ratio]]
  def writeRelativeStakes(epoch: Epoch, relativeStakes: Map[TaktikosAddress, Ratio]): F[Unit]
  def lookupRegistration(epoch:  Epoch)(address:        TaktikosAddress): F[Option[Box.Values.TaktikosRegistration]]
  def writeRegistrations(epoch:  Epoch, registrations:  Map[TaktikosAddress, Box.Values.TaktikosRegistration]): F[Unit]
  def lookupEta(epoch:           Epoch): F[Option[Eta]]
  def writeEta(epoch:            Epoch, eta:            Eta): F[Unit]
}
