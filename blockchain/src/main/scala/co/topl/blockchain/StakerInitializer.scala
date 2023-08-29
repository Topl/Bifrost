package co.topl.blockchain

import cats.implicits._
import co.topl.brambl.models._
import co.topl.brambl.models.box._
import co.topl.brambl.models.transaction.{IoTransaction, UnspentTransactionOutput}
import co.topl.consensus.models._
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.models.SecretKeyKesProduct
import co.topl.crypto.signing.Ed25519
import co.topl.crypto.signing.Ed25519VRF
import co.topl.crypto.signing.KesProduct
import co.topl.models._
import co.topl.models.utility._
import co.topl.models.utility.Lengths
import co.topl.models.utility.Sized
import com.google.protobuf.ByteString
import quivr.models._

/**
 * Represents the data required to initialize a new staking.  This includes the necessary secret keys, plus their
 * derived verification keys and addresses.
 */
sealed abstract class StakerInitializer

object StakerInitializers {

  /**
   * An initializer for an Operator.  An Operator needs the full suite of keys in order to perform its duties.
   * @param operatorSK The operator's registration key
   * @param lockAddress The operator's lock address to associate with the registration UTxO
   * @param vrfSK The operator's VRF/eligibility/PoS key
   * @param kesSK The operator's forward-secure key
   */
  case class Operator(
    operatorSK:  ByteString,
    lockAddress: LockAddress,
    vrfSK:       ByteString,
    kesSK:       SecretKeyKesProduct
  ) extends StakerInitializer {

    val vrfVK: Bytes = ByteString.copyFrom(Ed25519VRF.precomputed().getVerificationKey(vrfSK.toByteArray))

    val operatorVK: Bytes = ByteString.copyFrom(
      new Ed25519().getVerificationKey(Ed25519.SecretKey(operatorSK.toByteArray)).bytes
    )

    val registrationSignature: SignatureKesProduct =
      new KesProduct().sign(kesSK, new Blake2b256().hash(vrfVK, operatorVK))

    val stakingAddress: StakingAddress =
      StakingAddress(
        ByteString.copyFrom(
          new Ed25519().getVerificationKey(Ed25519.SecretKey(operatorSK.toByteArray)).bytes
        )
      )

    val registration: StakingRegistration =
      StakingRegistration(stakingAddress, registrationSignature)

    /**
     * This staker's initial stake in the network
     */
    def registrationTransaction(stake: Int128): IoTransaction = {
      val toplValue =
        Value.defaultInstance.withTopl(
          Value.TOPL(stake, StakingRegistration(stakingAddress, registrationSignature).some)
        )
      val outputs = List(
        UnspentTransactionOutput(lockAddress, toplValue)
      )
      IoTransaction(inputs = Nil, outputs = outputs, datum = Datum.IoTransaction.defaultInstance)
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

      val operatorSK = new Ed25519()
        .deriveKeyPairFromSeed(
          blake2b256.hash(seed.data.toByteArray :+ 1)
        )
        .signingKey
        .bytes

      val spendingLockAddress = PrivateTestnet.HeightLockOneSpendingAddress

      val (vrfSK, _) =
        Ed25519VRF
          .precomputed()
          .deriveKeyPairFromSeed(
            blake2b256.hash(seed.data.toByteArray :+ 2)
          )
      val (kesSK, _) = new KesProduct().createKeyPair(
        seed = blake2b256.hash(seed.data.toByteArray :+ 3),
        height = kesKeyHeight,
        0
      )
      Operator(
        ByteString.copyFrom(operatorSK),
        spendingLockAddress,
        ByteString.copyFrom(vrfSK),
        kesSK
      )
    }
  }

}
