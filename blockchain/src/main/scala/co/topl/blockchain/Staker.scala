package co.topl.blockchain

import cats.data.Chain
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.signing.{Ed25519, Ed25519VRF, KesProduct}
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models._
import co.topl.typeclasses.implicits._

sealed abstract class Staker

object Stakers {

  case class Operator(
    operatorSK: SecretKeys.Ed25519,
    walletSK:   SecretKeys.Ed25519,
    spendingSK: SecretKeys.Ed25519,
    vrfSK:      SecretKeys.VrfEd25519,
    kesSK:      SecretKeys.KesProduct
  ) extends Staker {

    val vrfVK: VerificationKeys.VrfEd25519 =
      Ed25519VRF.precomputed().getVerificationKey(vrfSK)

    val operatorVK: VerificationKeys.Ed25519 =
      Ed25519.instance.getVerificationKey(operatorSK)

    val registration: Box.Values.Registrations.Operator =
      Box.Values.Registrations.Operator(
        vrfCommitment = new KesProduct().sign(
          kesSK,
          new Blake2b256()
            .hash(vrfVK.immutableBytes, operatorVK.immutableBytes)
            .data
        )
      )

    val stakingAddress: StakingAddresses.Operator =
      StakingAddresses.Operator(Ed25519.instance.getVerificationKey(operatorSK))

    val spendingVK: VerificationKeys.Ed25519 =
      Ed25519.instance.getVerificationKey(spendingSK)

    val spendingAddress: SpendingAddress =
      spendingVK.spendingAddress

    def address(implicit networkPrefix: NetworkPrefix): FullAddress =
      FullAddress(
        networkPrefix = networkPrefix,
        spendingAddress = spendingAddress,
        stakingAddress = stakingAddress,
        binding = Ed25519.instance.sign(
          walletSK,
          spendingAddress.immutableBytes ++ stakingAddress.immutableBytes
        )
      )

    def bigBangOutputs(stake: Int128)(implicit networkPrefix: NetworkPrefix): Chain[Transaction.Output] =
      Chain(
        Transaction.Output(address, Box.Values.Arbit(stake), minting = true),
        Transaction.Output(address, registration, minting = true)
      )
  }

  object Operator {

    /**
     * Create an Operator Staker using the given seed and KES configuration
     * @param seed 32 bytes of seed data
     * @param kesKeyHeight KES Key Height
     * @return an Operator Staker
     */
    def apply(
      seed:         Sized.Strict[Bytes, Lengths.`32`.type],
      kesKeyHeight: (Int, Int)
    ): Operator = {
      // NOTE: To avoid SK collisions, each SK generated below is from
      // the hash of the given seed appended with a byte suffix
      val blake2b256 = new Blake2b256()

      val (operatorSK, _) = new Ed25519().deriveKeyPairFromSeed(
        blake2b256.hash(seed.data ++ Bytes.fromByte(1))
      )
      val (walletSK, _) = new Ed25519().deriveKeyPairFromSeed(
        blake2b256.hash(seed.data ++ Bytes.fromByte(2))
      )
      val (spendingSK, _) = new Ed25519().deriveKeyPairFromSeed(
        blake2b256.hash(seed.data ++ Bytes.fromByte(3))
      )
      val (vrfSK, _) =
        Ed25519VRF
          .precomputed()
          .deriveKeyPairFromSeed(
            blake2b256.hash(seed.data ++ Bytes.fromByte(3))
          )
      val (kesSK, _) = new KesProduct().createKeyPair(
        seed = blake2b256.hash(seed.data ++ Bytes.fromByte(4)).data,
        height = kesKeyHeight,
        0
      )
      Operator(operatorSK, walletSK, spendingSK, vrfSK, kesSK)
    }
  }

}
