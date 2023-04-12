package co.topl.blockchain

import cats.implicits._
import co.topl.brambl.common.ContainsEvidence
import co.topl.brambl.common.ContainsImmutable.instances.lockImmutable
import co.topl.brambl.models._
import co.topl.brambl.models.box._
import co.topl.brambl.models.transaction.UnspentTransactionOutput
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
import quivr.models.VerificationKey._

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
    vrfSK:      ByteString,
    kesSK:      SecretKeyKesProduct
  ) extends StakerInitializer {

    val vrfVK: Bytes = ByteString.copyFrom(Ed25519VRF.precomputed().getVerificationKey(vrfSK.toByteArray))

    val operatorVK: Bytes = ByteString.copyFrom(
      new Ed25519().getVerificationKey(Ed25519.SecretKey(operatorSK.toByteArray)).bytes
    )

    val registration: SignatureKesProduct =
      new KesProduct().sign(kesSK, new Blake2b256().hash(vrfVK, operatorVK).toArray)

    val stakingAddress: StakingAddress =
      StakingAddress(
        ByteString.copyFrom(
          new Ed25519().getVerificationKey(Ed25519.SecretKey(operatorSK.toByteArray)).bytes
        )
      )

    val spendingVK: ByteString = ByteString.copyFrom(
      new Ed25519().getVerificationKey(Ed25519.SecretKey(spendingSK.toByteArray)).bytes
    )

    val lockAddress: LockAddress =
      LockAddress(
        0,
        0,
        LockAddress.Id.Lock32(
          Identifier.Lock32(
            ContainsEvidence[Lock].sized32Evidence(
              Lock(
                Lock.Value.Predicate(
                  Lock.Predicate(
                    List(
                      Challenge().withRevealed(
                        Proposition(
                          Proposition.Value.DigitalSignature(
                            Proposition.DigitalSignature("ed25519", VerificationKey(Vk.Ed25519(Ed25519Vk(spendingVK))))
                          )
                        )
                      )
                    ),
                    1
                  )
                )
              )
            )
          )
        )
      )

    /**
     * This staker's initial stake in the network
     */
    def bigBangOutputs(stake: Int128)(implicit networkPrefix: NetworkPrefix): List[UnspentTransactionOutput] = {
      val toplValue = Value().withTopl(Value.TOPL(stake, stakingAddress.some))
      val registrationValue = Value().withRegistration(Value.Registration(registration, stakingAddress))
      List(
        UnspentTransactionOutput(lockAddress, toplValue),
        UnspentTransactionOutput(lockAddress, registrationValue)
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

      val operatorSK = new Ed25519()
        .deriveKeyPairFromSeed(
          blake2b256.hash(seed.data.toByteArray :+ 1)
        )
        .signingKey
        .bytes
      val walletSK = new Ed25519()
        .deriveKeyPairFromSeed(
          blake2b256.hash(seed.data.toByteArray :+ 2)
        )
        .signingKey
        .bytes
      val spendingSK = new Ed25519()
        .deriveKeyPairFromSeed(
          blake2b256.hash(seed.data.toByteArray :+ 3)
        )
        .signingKey
        .bytes
      val (vrfSK, _) =
        Ed25519VRF
          .precomputed()
          .deriveKeyPairFromSeed(
            blake2b256.hash(seed.data.toByteArray :+ 4)
          )
      val (kesSK, _) = new KesProduct().createKeyPair(
        seed = blake2b256.hash(seed.data.toByteArray :+ 5),
        height = kesKeyHeight,
        0
      )
      Operator(
        ByteString.copyFrom(operatorSK),
        ByteString.copyFrom(walletSK),
        ByteString.copyFrom(spendingSK),
        ByteString.copyFrom(vrfSK),
        kesSK
      )
    }
  }

}
