package co.topl.consensus

import cats.{Applicative, Monad}
import cats.implicits._
import co.topl.consensus.algebras.LeaderElectionValidationAlgebra
import co.topl.crypto.typeclasses.Signable
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.implicits._

import java.nio.charset.StandardCharsets

object LeaderElectionValidation {

  sealed abstract class Token {
    def bytes: Array[Byte]
  }

  object Tokens {

    case object Test extends Token {
      val bytes: Array[Byte] = "TEST".getBytes(StandardCharsets.UTF_8)
    }

    case object Nonce extends Token {
      val bytes: Array[Byte] = "NONCE".getBytes(StandardCharsets.UTF_8)
    }
  }

  case class VrfConfig(lddCutoff: Int, precision: Int, baselineDifficulty: Ratio, amplitude: Ratio)

  case class VrfArgument(eta: Eta, slot: Slot, token: Token)

  implicit val signableVrfArgument: Signable[VrfArgument] =
    arg => arg.eta.data ++ BigInt(arg.slot).toByteArray ++ arg.token.bytes

  object Eval {

    def make[F[_]: Monad](config: VrfConfig): LeaderElectionValidationAlgebra[F] =
      new LeaderElectionValidationAlgebra[F] {

        // TODO: Cache of relative stake for each address

        def getThreshold(relativeStake: Ratio, slotDiff: Long): F[Ratio] =
          Applicative[F].unit.productREval(
            cats.Eval.later {
              val mFValue = mFunction(slotDiff, config)
              val base = mFValue * relativeStake

              (1 to config.precision)
                .foldLeft(Ratio(0))((total, i) => total - (base.pow(i) * Ratio(BigInt(1), ProsomoMath.factorial(i))))
                .pure[F]
            }
          )

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

        /** Calculates log(1-f(slot-parentSlot)) or log(1-f) depending on the configuration */
        private def mFunction(slotDiff: Long, config: VrfConfig): Ratio =
          // use sawtooth curve if local dynamic difficulty is enabled
          if (slotDiff <= config.lddCutoff)
            ProsomoMath.logOneMinus(
              ProsomoMath.lddGapSawtooth(slotDiff, config.lddCutoff, config.amplitude),
              config.precision
            )
          else ProsomoMath.logOneMinus(config.baselineDifficulty, config.precision)

      }
  }
}

private object ProsomoMath {

  def factorial(n: Int): BigInt = (1 to n).product

  /** Calculates log(1-f) */
  def logOneMinus(f: Ratio, precision: Int): Ratio =
    (1 to precision).foldLeft(Ratio(0))((total, value) => total - (f.pow(value) / value))

  // Local Dynamic Difficulty curve
  def lddGapSawtooth(slotDiff: Long, lddCutoff: Int, amplitude: Ratio): Ratio =
    Ratio(BigInt(slotDiff), BigInt(lddCutoff)) * amplitude

}
