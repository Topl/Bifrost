package co.topl.consensus

import cats.Monad
import cats.implicits._
import co.topl.algebras.LeaderElectionAlgebra
import co.topl.consensus.vrf.ProofToHash
import co.topl.crypto.signatures.Ed25519VRF
import co.topl.crypto.typeclasses._
import co.topl.crypto.typeclasses.implicits._
import co.topl.typeclasses.implicits._
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Ratio, Sized}

import java.nio.charset.StandardCharsets

object LeaderElection {

  object Eval {

    def make[F[_]: Monad](secret: PrivateKeys.Vrf, config: Vrf.Config): LeaderElectionAlgebra[F] =
      new LeaderElectionAlgebra[F] {

        def getHit(
          relativeStake: Ratio,
          slot:          Slot,
          slotDiff:      Epoch,
          eta:           Eta
        ): F[Option[Vrf.Hit]] = {

          val privateKeyBytes = secret.ed25519.bytes.data

          val vrf = VrfProof(privateKeyBytes, eta, slot)

          getThreshold(relativeStake, slotDiff).flatMap(threshold =>
            isSlotLeaderForThreshold(threshold)(ProofToHash.digest(vrf.testProof)).map(isSlotLeader =>
              if (isSlotLeader)
                Vrf
                  .Hit(
                    Vrf.Certificate(
                      implicitly[ContainsVerificationKey[PrivateKeys.Vrf, PublicKeys.Vrf]].verificationKeyOf(secret),
                      vrf.nonceProof,
                      vrf.testProof
                    ),
                    slot,
                    threshold
                  )
                  .some
              else
                none[Vrf.Hit]
            )
          )
        }

        def getThreshold(relativeStake: Ratio, slotDiff: Long): F[Ratio] = {
          val mFValue = mFunction(slotDiff, config)
          val base = mFValue * relativeStake

          (1 to config.precision)
            .foldLeft(Ratio(0))((total, i) => total - (base.pow(i) * Ratio(BigInt(1), ProsomoMath.factorial(i))))
            .pure[F]
        }

        /**
         * Determines if the given proof meets the threshold to be elected slot leader
         * @param threshold the threshold to reach
         * @param proof the proof output
         * @return true if elected slot leader and false otherwise
         */
        def isSlotLeaderForThreshold(threshold: Ratio)(proofHash: Rho): F[Boolean] =
          (threshold > proofHash.data
            .zip(1 to proofHash.data.length) // zip with indexes starting from 1
            .foldLeft(Ratio(0)) { case (net, (byte, i)) =>
              net + Ratio(BigInt(byte & 0xff), BigInt(2).pow(8 * i))
            })
            .pure[F]

        def nextHit(
          relativeStake: Ratio,
          initialSlot:   Slot,
          maxSlot:       Epoch,
          eta:           Eta
        ): F[Option[Vrf.Hit]] =
          (initialSlot, None: Option[Vrf.Hit])
            .iterateUntilM { case (slot, _) =>
              getHit(relativeStake, slot + 1, slot + 1 - slot, eta)
                .map((slot + 1) -> _)
            } { case (slot, previousHit) =>
              slot > maxSlot || previousHit.nonEmpty
            }
            .map(_._2)
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
