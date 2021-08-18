package co.topl.consensus

import co.topl.consensus.crypto.{Ratio, Vrf}
import co.topl.models.{Bytes, Nonce}

object LeaderElection {

  sealed abstract class Failure

  object Failures {
    case class ThresholdNotMet(threshold: Ratio, proof: Proof) extends Failure
  }

  case class Key(privateKey: SecretKey, publicKey: PublicKey)

  case class Certificate(publicKey: PublicKey, proofHash: Hash, testProof: Proof, threshold: Ratio)

  case class Hit(cert: Certificate, proof: Proof)

  case class Config(lddCutoff: Int, precision: Int, baselineDifficulty: Ratio, amplitude: Ratio)

  /**
   * Gets if the given key is elected for the given slot.
   * @param key the key to stake with
   * @param relativeStake the key's relative stake in the chain
   * @param slot the current slot number
   * @param slotDiff the number of slots since the parent slot
   * @param epochNonce the current epoch's nonce
   * @param config configuration settings
   * @return a hit if the key has been elected for the slot
   */
  def getHit(
    key:           Key,
    relativeStake: Ratio,
    slot:          Slot,
    slotDiff:      Slot, // diff between current slot and parent slot
    epochNonce:    Nonce,
    config:        Config
  ): Either[Failure, Hit] = {
    // private key is 33 bytes, with the first being the type byte (unneeded)
    val privateKeyBytes = key.privateKey.dataBytes

    // create VRF for current state
    val vrf = VrfProof(privateKeyBytes, epochNonce, slot)

    // get the proof used for testing slot eligibility
    val proof = vrf.testProof

    val threshold = getThreshold(relativeStake, slotDiff, config)

    Either.cond(
      isSlotLeaderForThreshold(threshold)(proof),
      Hit(Certificate(key.publicKey, vrf.testProofHashed, proof, threshold), vrf.nonceProof),
      Failures.ThresholdNotMet(threshold, proof)
    )
  }

  /** Calculates log(1-f(slot-parentSlot)) or log(1-f) depending on the configuration */
  def mFunction(slotDiff: Int, config: Config): Ratio =
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
  def getThreshold(relativeStake: Ratio, slotDiff: Int, config: Config): Ratio = {
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
  def isSlotLeaderForThreshold(threshold: Ratio)(proof: Proof): Boolean =
    threshold > proof
      .zip(1 to proof.length) // zip with indexes starting from 1
      .foldLeft(Ratio(0)) { case (net, (byte, i)) =>
        net + Ratio(BigInt(byte & 0xff), BigInt(2).pow(8 * i))
      }

  case class VrfProof(vrf: Vrf, proofFunc: String => Proof) {
    lazy val testProof: Proof = proofFunc("TEST")
    lazy val nonceProof: Proof = proofFunc("NONCE")
    lazy val testProofHashed: Proof = hash(testProof.unsafeArray.asInstanceOf[Array[Byte]])

    def hash(input: Array[Byte]): Hash = Bytes(vrf.Sha512(input))
  }

  object VrfProof {

    def apply(secretData: Bytes, epochNonce: Nonce, slot: Slot): VrfProof = {
      val vrf = new Vrf()
      VrfProof(
        vrf,
        (token: String) =>
          Bytes(
            vrf.vrfProof(
              secretData.unsafeArray.asInstanceOf[Array[Byte]],
              (epochNonce ++ secretData ++ BigInt(slot).toByteArray ++ token.getBytes).toArray[Byte]
            )
          )
      )
    }
  }
}

object ProsomoMath {

  def factorial(n: Int): BigInt = (1 to n).product

  /** Calculates log(1-f) */
  def logOneMinus(f: Ratio, precision: Int): Ratio =
    (1 to precision).foldLeft(Ratio(0))((total, value) => total - (f.pow(value) / value))

  // Local Dynamic Difficulty curve
  def lddGapSawtooth(slotDiff: Int, lddCutoff: Int, amplitude: Ratio): Ratio = Ratio(slotDiff, lddCutoff) * amplitude

}
