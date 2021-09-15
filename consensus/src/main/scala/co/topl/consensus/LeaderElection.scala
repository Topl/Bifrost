package co.topl.consensus

import co.topl.consensus.vrf.ProofToHash
import co.topl.crypto.signatures.Ed25519VRF
import co.topl.models._
import co.topl.models.utility.HasLength.implicits._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Ratio, Sized}
import co.topl.typeclasses.RatioOps.implicits._
import co.topl.typeclasses.crypto.ContainsVerificationKey
import co.topl.typeclasses.crypto.ContainsVerificationKey.instances.vrfContainsVerificationKey

import java.nio.charset.StandardCharsets

object LeaderElection {

  sealed abstract class Failure

  object Failures {
    case class ThresholdNotMet(threshold: Ratio, proof: Proofs.Consensus.VrfTest) extends Failure
  }

  def hits(secret: PrivateKeys.Vrf, relativeStake: Ratio, fromSlot: Slot, untilSlot: Slot, epochNonce: Eta)(implicit
    config:        Vrf.Config
  ): Iterator[Vrf.Hit] =
    ((fromSlot + 1) until untilSlot).iterator
      .map(slot => getHit(secret, relativeStake, slot, slot - fromSlot, epochNonce))
      .collect { case Right(hit) => hit }

  /**
   * Gets if the given key is elected for the given slot.
   * @param secret the key to stake with
   * @param relativeStake the key's relative stake in the chain
   * @param slot the current slot number
   * @param slotDiff the number of slots since the parent slot
   * @param epochNonce the current epoch's nonce
   * @param config configuration settings
   * @return a hit if the key has been elected for the slot
   */
  def getHit(
    secret:          PrivateKeys.Vrf,
    relativeStake:   Ratio,
    slot:            Slot,
    slotDiff:        Long, // diff between current slot and parent slot
    epochNonce:      Eta
  )(implicit config: Vrf.Config): Either[Failure, Vrf.Hit] = {
    // private key is 33 bytes, with the first being the type byte (unneeded)
    val privateKeyBytes = secret.ed25519.bytes.data

    // create VRF for current state
    val vrf = VrfProof(privateKeyBytes, epochNonce, slot)
    val threshold = getThreshold(relativeStake, slotDiff)

    Either.cond(
      isSlotLeaderForThreshold(threshold)(ProofToHash.digest(vrf.testProof)),
      Vrf.Hit(
        Vrf.Certificate(
          implicitly[ContainsVerificationKey[PrivateKeys.Vrf, PublicKeys.Vrf]].verificationKeyOf(secret),
          vrf.nonceProof,
          vrf.testProof
        ),
        slot,
        threshold
      ),
      Failures.ThresholdNotMet(threshold, vrf.testProof)
    )
  }

  /** Calculates log(1-f(slot-parentSlot)) or log(1-f) depending on the configuration */
  def mFunction(slotDiff: Long, config: Vrf.Config): Ratio =
    // use sawtooth curve if local dynamic difficulty is enabled
    if (slotDiff <= config.lddCutoff)
      ProsomoMath.logOneMinus(
        ProsomoMath.lddGapSawtooth(slotDiff, config.lddCutoff, config.amplitude),
        config.precision
      )
    else ProsomoMath.logOneMinus(config.baselineDifficulty, config.precision)

  /**
   * Gets the required threshold for the given parameters.
   * @param relativeStake the relative stake in the system
   * @param slotDiff the number of slots between the current slot and the parent slot
   * @param config configuration settings
   * @return the election threshold
   */
  def getThreshold(relativeStake: Ratio, slotDiff: Long)(implicit config: Vrf.Config): Ratio = {
    val mFValue = mFunction(slotDiff, config)
    val base = mFValue * relativeStake

    (1 to config.precision)
      .foldLeft(Ratio(0))((total, i) => total - (base.pow(i) * Ratio(BigInt(1), ProsomoMath.factorial(i))))
  }

  /**
   * Determines if the given proof meets the threshold to be elected slot leader
   * @param threshold the threshold to reach
   * @param proof the proof output
   * @return true if elected slot leader and false otherwise
   */
  def isSlotLeaderForThreshold(threshold: Ratio)(proofHash: Rho): Boolean =
    threshold > proofHash.data
      .zip(1 to proofHash.data.length) // zip with indexes starting from 1
      .foldLeft(Ratio(0)) { case (net, (byte, i)) =>
        net + Ratio(BigInt(byte & 0xff), BigInt(2).pow(8 * i))
      }

  case class VrfProof(vrf: Ed25519VRF, proofFunc: String => Bytes) {
    lazy val testProof: Proofs.Consensus.VrfTest = Proofs.Consensus.VrfTest(Sized.strictUnsafe(proofFunc("TEST")))
    lazy val nonceProof: Proofs.Consensus.Nonce = Proofs.Consensus.Nonce(Sized.strictUnsafe(proofFunc("NONCE")))
  }

  object VrfProof {
    val vrf = new Ed25519VRF
    vrf.precompute()

    def apply(secretData: Bytes, eta: Eta, slot: Slot): VrfProof =
      VrfProof(
        vrf,
        (token: String) =>
          Bytes(
            vrf.vrfProof(
              secretData.toArray,
              eta.data.toArray ++ BigInt(slot).toByteArray ++ token.getBytes(StandardCharsets.UTF_8)
            )
          )
      )
  }
}

object ProsomoMath {

  def factorial(n: Int): BigInt = (1 to n).product

  /** Calculates log(1-f) */
  def logOneMinus(f: Ratio, precision: Int): Ratio =
    (1 to precision).foldLeft(Ratio(0))((total, value) => total - (f.pow(value) / value))

  // Local Dynamic Difficulty curve
  def lddGapSawtooth(slotDiff: Long, lddCutoff: Int, amplitude: Ratio): Ratio =
    Ratio(BigInt(slotDiff), BigInt(lddCutoff)) * amplitude

}
