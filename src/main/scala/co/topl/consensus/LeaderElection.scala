package co.topl.consensus

import co.topl.attestation.Address
import co.topl.attestation.keyManagement.PrivateKeyCurve25519
import co.topl.consensus.LeaderElectionProsomo.{Proof, isSlotLeaderForThreshold}
import co.topl.cryptoprimitives.{Ratio, Vrf}
import co.topl.modifier.block.Block
import co.topl.modifier.box.{ArbitBox, ProgramId}
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.state.StateReader
import co.topl.utils.{Logging, TimeProvider}
import com.google.common.primitives.Longs
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.Curve25519

import scala.concurrent.duration.MILLISECONDS
import scala.math.BigInt
import scala.util.Random

object LeaderElectionBifrost extends Logging {

  type TX = Transaction.TX

  type Hit = ArbitBox

  type SR = StateReader[ProgramId, Address]

  def getHit(parent: Block, addresses: Set[Address], timestamp: TimeProvider.Time, stateReader: SR
            ): Either[NoArbitBoxesError, Option[Hit]] = {
    val arbitBoxes = getArbitBoxes(stateReader, addresses)

    val adjustedTarget: BigDecimal = {
      val target: Double = parent.difficulty.toDouble / consensusStorage.totalStake.toDouble
      val timeDelta = timestamp - parent.timestamp

      BigDecimal(target * timeDelta.toDouble / targetBlockTime(parent.height).toUnit(MILLISECONDS))
    }

    def calculateHit(blockBytes: Array[Byte], boxBytes: Array[Byte]): Long = {
      val hash = Blake2b256(blockBytes ++ boxBytes)

      Longs.fromByteArray((0: Byte) +: hash.take(7))
    }

    // test procedure to determine eligibility
    arbitBoxes
      .map(
        _.map(box => (box, calculateHit(parent.bytes, box.bytes)))
          .filter { test =>
            BigInt(test._2) < (test._1.value.quantity.doubleValue() * adjustedTarget).toBigInt
          }
          .map(h => h._1)
          .headOption)
  }

  def getArbitBoxes(stateReader: SR, addresses: Set[Address]): Either[NoArbitBoxesError, Seq[ArbitBox]] =
    if (addresses.nonEmpty) {
      val boxes = addresses.flatMap {
        stateReader
          .getTokenBoxes(_)
          .getOrElse(Seq())
          .collect { case box: ArbitBox => box }
      }.toSeq

      Right(boxes)
    } else {
      Left(NoArbitBoxesError())
    }

  case class NoArbitBoxesError()
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

  val defaultLddCutoff = 15
  val precision = 16
  val baselineDifficulty = Ratio(1, 25)

  def getHit(
    key: Key,
    relativeStake: Ratio,
    slot: Slot,
    slotDiff: Slot, // diff between current slot and parent slot
    parentProof: Proof,
    epochNonce: Nonce,
    lddCutoff: Slot = defaultLddCutoff
  ): Option[Hit] = {
    println(s"Slot diff is '${slotDiff}'")

    val privateKeyBytes = key.privateKey.tail
    val vrf = VrfProof(privateKeyBytes, epochNonce, slot)

    val testProof = vrf.testProof

    val threshold = getThreshold(relativeStake, slotDiff, lddCutoff)

    println(s"Threshold is '${threshold.toDouble}'")

    val isSlotLeader = isSlotLeaderForThreshold(threshold) _

    if (isSlotLeader(testProof, parentProof, slotDiff, lddCutoff, vrf.hash))
      // TODO: add meta info
      Some(Hit(Certificate(key.publicKey, vrf.testProofHashed, testProof, threshold), vrf.nonceProof))
    else None
  }

  /** log(1-f(slot-parentSlot)) */
  def mFunction(slotDiff: Int, lddCutoff: Int): Ratio =
    if (slotDiff <= lddCutoff)
      ProsomoMath.logOneMinus(ProsomoMath.lddGapSawtooth(slotDiff, lddCutoff))
    else ProsomoMath.logOneMinus(baselineDifficulty)

  def getThreshold(relativeStake: Ratio, slotDiff: Int, lddCutoff: Int): Ratio = {
    val mFValue = mFunction(slotDiff, lddCutoff)
    val base = mFValue * relativeStake

    (1 to precision)
      .foldLeft(Ratio(0))((total, i) => total - (base.pow(i) * Ratio(BigInt(1), ProsomoMath.factorial(i))))
  }

  def isSlotLeaderForThreshold(threshold: Ratio)(
    testProof: Proof,
    parentProof: Proof,
    slotDiff: Int,
    lddCutoff: Int,
    hash: Array[Byte] => Hash
  ): Boolean = {

    // I don't understand what this is for
    val testStakeProof = if (slotDiff <= lddCutoff) hash(testProof ++ parentProof) else testProof

    threshold > testStakeProof
      .zip(1 to testStakeProof.length) // zip with indexes starting from 1
      .foldLeft(Ratio(0)) {
        case (net, (byte, i)) =>
          net + Ratio(BigInt(byte & 0xff), BigInt(2).pow(8 * i))
      }

  }

  case class VrfProof(vrf: Vrf, proofFunc: String => Proof) {
    lazy val testProof: Proof = proofFunc("TEST")
    lazy val nonceProof: Proof = proofFunc("NONCE")
    lazy val testProofHashed: Proof = testProof

    def hash(input: Array[Byte]): Hash = vrf.Sha512(input)
  }

  object VrfProof {
    def apply(secret: SecretKey, epochNonce: Nonce, slot: Slot): VrfProof = {
      val vrf = new Vrf()
      VrfProof(
        vrf,
        (token: String) =>
          vrf.vrfProof(secret, epochNonce ++ secret ++ BigInt(slot).toByteArray ++ token.getBytes))
    }
  }
}

object LeaderElectionTester extends App {
  val random = new Random(10)
  def randomBytes(l: Int): Array[Byte] = Array.fill(l)((random.nextInt(256) - 128).toByte)

  val key = {
    val k = PrivateKeyCurve25519.secretGenerator.generateSecret(randomBytes(Curve25519.KeyLength))
    LeaderElectionProsomo.Key(k._2.bytes, k._1.bytes)
  }

  val relativeStake = Ratio(1, 5)
  val parentProof = randomBytes(32)
  val epochNonce = randomBytes(32)

  // test slots 101 - 200
  // parent has slot 100
  (1 to 100).foreach(x => {
    val result = LeaderElectionProsomo.getHit(
      key,
      relativeStake,
      100 + x,
      x,
      parentProof,
      epochNonce
    )

    println(result)
  })
}

object ProsomoMath {

  val precision: Int = 16
  val amplitude = Ratio(2, 5)

  def factorial(n: Int): BigInt = (1 to n).product

  /** Calculates log(1-f) */
  def logOneMinus(f: Ratio): Ratio =
    (1 to precision).foldLeft(Ratio(0))((total, value) => total - (f.pow(value) / value))

  // Local Dynamic Difficulty curve
  def lddGapSawtooth(slotDiff: Int, lddCutoff: Int): Ratio = Ratio(slotDiff, lddCutoff) * amplitude

}
