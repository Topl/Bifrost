package co.topl.blockchain

import cats.data.Chain
import cats.implicits._
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.crypto.signing.{Ed25519VRF, KesProduct}
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.signing.Ed25519
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models._
import co.topl.models.utility._
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import quivr.models.Int128

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
    operatorSK: ByteString,
    walletSK:   ByteString,
    spendingSK: ByteString,
    vrfSK:      SecretKeys.VrfEd25519,
    kesSK:      SecretKeys.KesProduct
  ) extends StakerInitializer {

    val vrfVK: Bytes = Ed25519VRF.precomputed().getVerificationKey(vrfSK.bytes.data)
    val operatorVK: Bytes = new Ed25519().getVerificationKey(operatorSK)

    val registration =
      new KesProduct().sign(
        kesSK,
        new Blake2b256().hash(vrfVK, operatorVK)
      )

    val stakingAddress =
      new Ed25519().getVerificationKey(operatorSK)

    val spendingVK =
      new Ed25519().getVerificationKey(spendingSK)

    val spendingAddress: SpendingAddress =
      spendingVK.spendingAddress

    /**
     * This staker's initial stake in the network
     */
    def bigBangOutputs(stake: Int128)(implicit networkPrefix: NetworkPrefix): Chain[UnspentTransactionOutput] = {
      val value = Value().withTopl(Value.TOPL(stake, registration.some))
      Chain(
        UnspentTransactionOutput(address, Box.Values.Arbit(stake), minting = true),
        UnspentTransactionOutput(address, registration, minting = true)
      )
    }
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
        blake2b256.hash(seed.data :+ 1)
      )
      val (walletSK, _) = new Ed25519().deriveKeyPairFromSeed(
        blake2b256.hash(seed.data :+ 2)
      )
      val (spendingSK, _) = new Ed25519().deriveKeyPairFromSeed(
        blake2b256.hash(seed.data :+ 3)
      )
      val (vrfSK, _) =
        Ed25519VRF
          .precomputed()
          .deriveKeyPairFromSeed(
            blake2b256.hash(seed.data :+ 4)
          )
      val (kesSK, _) = new KesProduct().createKeyPair(
        seed = blake2b256.hash(seed.data :+ 5),
        height = kesKeyHeight,
        0
      )
      Operator(operatorSK, walletSK, spendingSK, vrfSK, kesSK)
    }
  }

}
