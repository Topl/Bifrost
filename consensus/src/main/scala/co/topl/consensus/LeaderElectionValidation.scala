package co.topl.consensus

import cats.Monad
import cats.implicits._
import co.topl.algebras.UnsafeResource
import co.topl.codecs.bytes.typeclasses.Signable
import co.topl.consensus.algebras.LeaderElectionValidationAlgebra
import co.topl.crypto.hash.Blake2b512
import co.topl.crypto.signing.Ed25519VRF
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.implicits._

import scala.collection.concurrent.TrieMap

object LeaderElectionValidation {

  case class VrfConfig(lddCutoff: Int, precision: Int, baselineDifficulty: Ratio, amplitude: Ratio)

  case class VrfArgument(eta: Eta, slot: Slot)

  implicit val signableVrfArgument: Signable[VrfArgument] =
    arg => arg.eta.data ++ Bytes(BigInt(arg.slot).toByteArray)

  object Eval {

    def make[F[_]: Monad](
      config:             VrfConfig,
      blake2b512Resource: UnsafeResource[F, Blake2b512]
    ): LeaderElectionValidationAlgebra[F] =
      new LeaderElectionValidationAlgebra[F] {

        // TODO: Cache of relative stake for each address

        def getThreshold(relativeStake: Ratio, slotDiff: Long): F[Ratio] = {
          val mFValue = mFunction(slotDiff, config)
          val base = mFValue * relativeStake

          // TODO: Where does this come from?
          (1 to config.precision)
            .foldLeft(Ratio(0))((total, i) => total - (base.pow(i) * Ratio(BigInt(1), MathUtils.factorial(i))))
            .pure[F]
        }

        /**
         * Determines if the given proof meets the threshold to be elected slot leader
         * @param threshold the threshold to reach
         * @param proof the proof output
         * @return true if elected slot leader and false otherwise
         */
        def isSlotLeaderForThreshold(threshold: Ratio)(rho: Rho): F[Boolean] = blake2b512Resource.use {
          implicit blake2b512 =>
            val testRhoHash = Ed25519VRF.rhoToRhoTestHash(rho)
            val testRhoHashBytes = testRhoHash.sizedBytes.data
            // TODO: Where does this come from?
            (threshold > testRhoHashBytes.toIterable
              .zip(1 to testRhoHashBytes.length.toInt) // zip with indexes starting from 1
              .foldLeft(Ratio(0)) { case (net, (byte, i)) =>
                net + Ratio(BigInt(byte & 0xff), BigInt(2).pow(8 * i))
              })
              .pure[F]
        }

        /** Calculates log(1-f(slot-parentSlot)) or log(1-f) depending on the configuration */
        private def mFunction(slotDiff: Long, config: VrfConfig): Ratio =
          // use sawtooth curve if local dynamic difficulty is enabled
          if (slotDiff <= config.lddCutoff)
            MathUtils.logOneMinus(
              MathUtils.lddGapSawtooth(slotDiff, config.lddCutoff, config.amplitude),
              config.precision
            )
          else MathUtils.logOneMinus(config.baselineDifficulty, config.precision)

      }
  }
}

private object MathUtils {

  private val factorialCache = TrieMap(0 -> BigInt(1))
  def factorial(n: Int): BigInt = factorialCache.getOrElseUpdate(n, n * factorial(n - 1))

  /** Calculates log(1-f) */
  def logOneMinus(f: Ratio, precision: Int): Ratio =
    (1 to precision).foldLeft(Ratio(0))((total, value) => total - (f.pow(value) / value))

  // Local Dynamic Difficulty curve
  def lddGapSawtooth(slotDiff: Long, lddCutoff: Int, amplitude: Ratio): Ratio =
    Ratio(BigInt(slotDiff), BigInt(lddCutoff)) * amplitude

}
