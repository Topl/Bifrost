package co.topl.minting

import co.topl.models._

trait StakingKeys[F[_]] {
  def vrfKey(): F[Secrets.Ed25519]
  def kesKey(): F[Secrets.Ed25519]
  def stakingAddress(): F[TaktikosAddress]
  def evolveKes(): F[Unit]
}
