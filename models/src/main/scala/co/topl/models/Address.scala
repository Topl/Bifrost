package co.topl.models

import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized
import co.topl.models.utility._
import com.google.protobuf.ByteString

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

  def operatorFromProtoString(address: ByteString): Operator =
    Operator(
      VerificationKeys.Ed25519(
        Sized.strictUnsafe[Bytes, VerificationKeys.Ed25519.Length](address)
      )
    )
}
