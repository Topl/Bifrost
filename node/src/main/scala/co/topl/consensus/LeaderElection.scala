package co.topl.consensus

import co.topl.attestation.Address
import co.topl.attestation.keyManagement.PrivateKeyCurve25519
import co.topl.consensus.LeaderElectionProsomo.Config
import co.topl.consensus.crypto.{Ratio, Vrf}
import co.topl.crypto.signatures.Curve25519
import co.topl.modifier.block.Block
import co.topl.modifier.box.{ArbitBox, ProgramId}
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.state.StateReader
import co.topl.utils.{Logging, TimeProvider}

import scala.collection.Set
import scala.util.Random

object LeaderElection extends Logging {

  type TX = Transaction.TX
  type SR = StateReader[ProgramId, Address]

  /**
   * Gets an arbit box that is eligible for forging the next block if there are any.
   * @param parent the parent block
   * @param addresses the addresses to stake with
   * @param timestamp the current time
   * @param stateReader a read-only version of state
   * @return an eligible box if one is found
   */
  def getEligibleBox(
    parent:      Block,
    addresses:   Set[Address],
    timestamp:   TimeProvider.Time,
    stateReader: SR
  ): Either[IneligibilityReason, ArbitBox] =
    if (addresses.isEmpty) {
      Left(NoAddressesAvailable)
    } else {
      val arbitBoxes = addresses.flatMap {
        stateReader
          .getTokenBoxes(_)
          .getOrElse(Seq())
          .collect { case box: ArbitBox => box }
      }.toSeq

      (arbitBoxes match {
        case Seq() => Left(NoArbitBoxesAvailable)
        case seq   => Right(seq)
      }).flatMap { boxes =>
        boxes
          .map(box => (box, calcHit(parent)(box)))
          .filter { case (box, hit) =>
            BigInt(hit) < calcTarget(box.value.quantity, timestamp - parent.timestamp, parent.difficulty, parent.height)
          }
          .map(_._1)
          .headOption
          .toRight(NoBoxesEligible)
      }
    }

  sealed trait IneligibilityReason
  case object NoAddressesAvailable extends IneligibilityReason
  case object NoBoxesEligible extends IneligibilityReason
  case object NoArbitBoxesAvailable extends IneligibilityReason
}

object LeaderElectionProsomo extends Logging {

  type Slot = Int
  type Proof = Array[Byte]
  type Hash = Array[Byte]
  type Nonce = Array[Byte]
  type SecretKey = Array[Byte]
  type PublicKey = Array[Byte]

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
  ): Option[Hit] = {
    // private key is 33 bytes, with the first being the type byte (unneeded)
    val privateKeyBytes = key.privateKey.tail

    // create VRF for current state
    val vrf = VrfProof(privateKeyBytes, epochNonce, slot)

    // get the proof used for testing slot eligibility
    val proof = vrf.testProof

    val threshold = getThreshold(relativeStake, slotDiff, config)

    if (isSlotLeaderForThreshold(threshold)(proof))
      Some(Hit(Certificate(key.publicKey, vrf.testProofHashed, proof, threshold), vrf.nonceProof))
    else None
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
    lazy val testProofHashed: Proof = hash(testProof)

    def hash(input: Array[Byte]): Hash = vrf.Sha512(input)
  }

  object VrfProof {

    def apply(secret: SecretKey, epochNonce: Nonce, slot: Slot): VrfProof = {
      val vrf = new Vrf()
      VrfProof(
        vrf,
        (token: String) => vrf.vrfProof(secret, epochNonce ++ secret ++ BigInt(slot).toByteArray ++ token.getBytes)
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

object LeaderElectionTester extends App {
  val random = new Random(100)
  def randomBytes(l: Int): Array[Byte] = Array.fill(l)((random.nextInt(256) - 128).toByte)

  val key = {
    val k = PrivateKeyCurve25519.secretGenerator.generateSecret(randomBytes(Curve25519.KeyLength))
    LeaderElectionProsomo.Key(k._2.bytes, k._1.bytes)
  }

  val relativeStake = Ratio(9, 10)
  val epochNonce = randomBytes(32)

  // test out hits on slots 1 to 500
  val numberHits = (1 to 300)
    .map(x =>
      LeaderElectionProsomo.getHit(
        key,
        relativeStake,
        x,
        x,
        epochNonce,
        Config(0, 16, Ratio(1, 15), Ratio(2, 5))
      )
    )
    .count(_.isDefined)

  println(numberHits)
}
