package co.topl.consensus

import co.topl.attestation.keyManagement.PrivateKeyCurve25519
import co.topl.cryptoprimitives.{Ratio, Vrf}
import co.topl.modifier.block.Block
import co.topl.modifier.box.ArbitBox
import co.topl.modifier.transaction.Transaction
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

  def getHit(parent: Block, arbitBoxes: Seq[ArbitBox], timestamp: TimeProvider.Time): Option[Hit] = {
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
      .map(box => (box, calculateHit(parent.bytes, box.bytes)))
      .filter { test =>
        BigInt(test._2) < (test._1.value.quantity.doubleValue() * adjustedTarget).toBigInt
      }
      .map(h => h._1)
      .headOption
  }
}

object LeaderElectionProsomo extends Logging {

  type Slot = Int
  type Proof = Array[Byte]
  type Hash = Array[Byte]
  type Nonce = Array[Byte]
  type SecretKey = Array[Byte]
  type PublicKey = Array[Byte]

  case class Key(privateKey: SecretKey, publicKey: PublicKey)

  case class Certificate(publicKey: PublicKey, proofHash: Hash, testProof: Proof, threshold: Ratio, metaInfo: String)

  case class Hit(cert: Certificate, proof: Proof)

  def getHit(
    key: Key,
    relativeStake: Ratio,
    slot: Slot,
    slotDiff: Slot, // diff between current slot and parent slot
    parentProof: Proof,
    vrfBuilder: (SecretKey, Slot, Nonce) => VrfProof,
    hash: Array[Byte] => Array[Byte],
    epochNonce: Nonce, // unknown value
    lddCutoff: Slot = 40,
    convergentAccuracy: Int = 16
  ): Option[Hit] = {
    val vrf = vrfBuilder(key.privateKey, slot, epochNonce)

    val testProof = vrf.testProof

    val threshold = getThreshold(relativeStake, mF(slotDiff), convergentAccuracy)
    val isSlotLeader = isSlotLeaderForThreshold(threshold) _

    // TODO: add meta info
    if (isSlotLeader(testProof, parentProof, slotDiff, lddCutoff, hash))
      Some(Hit(Certificate(key.publicKey, vrf.testProofHashed, testProof, threshold, ""), vrf.nonceProof))
    else None
  }

  // coefficient log(1-f(slot-parentSlot))
  def mF(slotDiff: Int): Ratio = {
    Ratio(1, 2)
  }

  def getThreshold(relativeStake: Ratio, mF: Ratio, convergentAccuracy: Int = 16): Ratio =
    (1 to convergentAccuracy)
      .foldLeft(Ratio(0)) { (total, i) =>
        // TODO: digest and verify what this is doing
        total - (mF * relativeStake).pow(i) * Ratio(BigInt(1), ProsomoMath.factorial(i))
      }

  def isSlotLeaderForThreshold(threshold: Ratio)(
    testProof: Proof,
    parentProof: Proof,
    slotDiff: Int,
    lddCutoff: Int,
    hash: Array[Byte] => Array[Byte]
  ): Boolean = {

    // I don't understand what this is for
    val testStakeProof = if (slotDiff <= lddCutoff) hash(testProof ++ parentProof) else testProof

    val attempt = testStakeProof
      .zip(1 to testStakeProof.length) // zip with indexes starting from 1
      .foldLeft(Ratio(0)) {
        case (net, (byte, i)) =>
          // TODO: digest and verify what this is doing
          net + Ratio(BigInt(byte & 0xff), BigInt(2).pow(8 * i))
      }

    attempt < threshold

  }

  trait VrfProof {
    def testProof: Proof
    def testProofHashed: Proof
    def nonceProof: Proof
  }
}

object LeaderElectionTester extends App {
  val random = new Random(1)
  def randomBytes(l: Int): Array[Byte] = Array.fill(l)((random.nextInt(256) - 128).toByte)

  val vrf = new Vrf()

  val key = {
    val k = PrivateKeyCurve25519.secretGenerator.generateSecret(randomBytes(Curve25519.KeyLength))
    LeaderElectionProsomo.Key(k._2.bytes, k._1.bytes)
  }
  val relativeStake = Ratio(1, 2)
  val slot = 20
  val slotDiff = 10
  val parentProof = randomBytes(32)
  val vrfBuilder =
    (secretKey: LeaderElectionProsomo.SecretKey, slot: LeaderElectionProsomo.Slot, eta: LeaderElectionProsomo.Nonce) =>
        new LeaderElectionProsomo.VrfProof {
          val proof: String => LeaderElectionProsomo.Proof = (customizer: String) =>
            vrf.vrfProof(secretKey.tail, eta ++ secretKey ++ BigInt(slot).toByteArray ++ customizer.getBytes)

          val testProof: LeaderElectionProsomo.Proof = proof("TEST")
          val testProofHashed: Array[Byte] = vrf.vrfProofToHash(testProof)
          val nonceProof: LeaderElectionProsomo.Proof = proof("NONCE")
        }
  val eta = randomBytes(32)

  val result = LeaderElectionProsomo.getHit(
    key,
    relativeStake,
    slot,
    slotDiff,
    parentProof,
    vrfBuilder,
    vrf.Sha512,
    eta
  )

  println(result)
}

object ProsomoMath {
  
  def factorial(n: Int): BigInt = (1 to n).product

}
