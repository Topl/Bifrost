package co.topl.models

case class FullAddress(
  networkPrefix:   NetworkPrefix,
  spendingAddress: SpendingAddress,
  stakingAddress:  StakingAddress,
  binding:         Proofs.Knowledge.Ed25519
)

case class SpendingAddress(typedEvidence: TypedEvidence)

sealed abstract class StakingAddress

object StakingAddresses {
  case class Operator(vk: VerificationKeys.Ed25519) extends StakingAddress
  // TODO
//  case class Delegating(vk: VerificationKeys.Ed25519) extends StakingAddress
  case object NonStaking extends StakingAddress
}
