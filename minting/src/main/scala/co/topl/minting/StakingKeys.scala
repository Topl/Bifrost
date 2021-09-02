package co.topl.minting

import co.topl.models._

/**
 * A provider of keys required for the staking procedure
 */
trait StakingKeys[F[_]] {
  def vrfKey: F[Secrets.Ed25519]
  def kesKey: F[Secrets.Ed25519]
  def stakingAddress: F[TaktikosAddress]

  /**
   * Returns a new `StakingKeys` where the KES key has been evolved
   */
  def evolved(): F[this.type]
}
