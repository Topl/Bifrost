package co.topl.blockchain

import cats.data.Chain
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.crypto.signing.{Ed25519VRF, KesProduct}
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.signing.Ed25519
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.typeclasses.implicits._

/**
 * Represents the data required to initialize a new staking.  This includes the necessary secret keys, plus their
 * derived verification keys and addresses.
 */
sealed abstract class StakerInitializer

object StakerInitializers {

  /**
   * An initializer for an Operator.  An Operator needs the full suite of keys in order to perform its duties.
   * @param operatorSK The operator's registration key
   * @param walletSK The operator's offline wallet key
   * @param spendingSK The operator's key with authority to spend its stake and registration
   * @param vrfSK The operator's VRF/eligibility/PoS key
   * @param kesSK The operator's forward-secure key
   */
  case class Operator(
    operatorSK: SecretKeys.Ed25519,
    walletSK:   SecretKeys.Ed25519,
    spendingSK: SecretKeys.Ed25519,
    vrfSK:      SecretKeys.VrfEd25519,
    kesSK:      SecretKeys.KesProduct
  ) extends StakerInitializer {

    val vrfVK: VerificationKeys.VrfEd25519 =
      VerificationKeys.VrfEd25519(Sized.strictUnsafe(Ed25519VRF.precomputed().getVerificationKey(vrfSK.bytes.data)))

    val operatorVK: VerificationKeys.Ed25519 =
      VerificationKeys.Ed25519(Sized.strictUnsafe(new Ed25519().getVerificationKey(operatorSK.bytes.data)))

    val registration: Box.Values.Registrations.Operator = {
      val vrfCommitment = new KesProduct().sign(
        kesSK,
        new Blake2b256()
          .hash(vrfVK.immutableBytes, operatorVK.immutableBytes)
      )
      // TODO, remove replace model util once operator refactor is done
      Box.Values.Registrations.Operator(co.topl.crypto.models.ReplaceModelUtil.signatureKesProduct(vrfCommitment))
    }

    val stakingAddress: StakingAddresses.Operator =
      StakingAddresses.Operator(
        VerificationKeys.Ed25519(Sized.strictUnsafe(new Ed25519().getVerificationKey(operatorSK.bytes.data)))
      )

    val spendingVK: VerificationKeys.Ed25519 =
      VerificationKeys.Ed25519(Sized.strictUnsafe(new Ed25519().getVerificationKey(spendingSK.bytes.data)))

    val spendingAddress: SpendingAddress =
      spendingVK.spendingAddress

    def address(implicit networkPrefix: NetworkPrefix): FullAddress =
      FullAddress(
        networkPrefix = networkPrefix,
        spendingAddress = spendingAddress,
        stakingAddress = stakingAddress,
        binding = Proofs.Knowledge.Ed25519(
          Sized.strictUnsafe(
            new Ed25519().sign(
              walletSK.bytes.data,
              spendingAddress.immutableBytes ++ stakingAddress.immutableBytes
            )
          )
        )
      )

    /**
     * This staker's initial stake in the network
     */
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

      val (operatorSKBytes, _) = new Ed25519().deriveKeyPairFromSeed(
        blake2b256.hash(seed.data :+ 1)
      )
      val operatorSK = SecretKeys.Ed25519(Sized.strictUnsafe(operatorSKBytes))
      val (walletSKBytes, _) = new Ed25519().deriveKeyPairFromSeed(
        blake2b256.hash(seed.data :+ 2)
      )
      val walletSK = SecretKeys.Ed25519(Sized.strictUnsafe(walletSKBytes))
      val (spendingSKBytes, _) = new Ed25519().deriveKeyPairFromSeed(
        blake2b256.hash(seed.data :+ 3)
      )
      val spendingSK = SecretKeys.Ed25519(Sized.strictUnsafe(spendingSKBytes))
      val (vrfSKBytes, _) =
        Ed25519VRF
          .precomputed()
          .deriveKeyPairFromSeed(
            blake2b256.hash(seed.data :+ 4)
          )
      val vrfSK = SecretKeys.VrfEd25519(Sized.strictUnsafe(vrfSKBytes))
      val (kesSK, _) = new KesProduct().createKeyPair(
        seed = blake2b256.hash(seed.data :+ 5),
        height = kesKeyHeight,
        0
      )
      Operator(operatorSK, walletSK, spendingSK, vrfSK, kesSK)
    }
  }

}
