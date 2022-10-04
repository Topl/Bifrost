package co.topl.consensus

import cats.implicits._
import cats.effect.{Clock, Sync}
import co.topl.algebras.{Exp, Log1p, UnsafeResource}
import co.topl.codecs.bytes.typeclasses.Signable
import co.topl.consensus.algebras.LeaderElectionValidationAlgebra
import co.topl.crypto.hash.Blake2b512
import co.topl.crypto.signing.Ed25519VRF
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.implicits._
import scalacache.caffeine.CaffeineCache

object LeaderElectionValidation {

  case class VrfConfig(lddCutoff: Int, precision: Int, baselineDifficulty: Ratio, amplitude: Ratio)

  case class VrfArgument(eta: Eta, slot: Slot)

  implicit val signableVrfArgument: Signable[VrfArgument] =
    arg => arg.eta.data ++ Bytes(BigInt(arg.slot).toByteArray)

  object Eval {

    /**
     * Normalization constant for test nonce hash evaluation based on 512 byte hash function output
     */
    private val NormalizationConstant: BigInt = BigInt(2).pow(512)

    def make[F[_]: Sync](
      config:             VrfConfig,
      blake2b512Resource: UnsafeResource[F, Blake2b512],
      exp:                Exp[F],
      log1p:              Log1p[F]
    ): LeaderElectionValidationAlgebra[F] =
      new LeaderElectionValidationAlgebra[F] {

        def getThreshold(relativeStake: Ratio, slotDiff: Long): F[Ratio] = {

          val difficultyCurve: Ratio =
            if (slotDiff > config.lddCutoff) config.baselineDifficulty
            else Ratio(BigInt(slotDiff), BigInt(config.lddCutoff)) * config.amplitude

          if (difficultyCurve == Ratio.One) {
            Ratio.One.pure[F]
          } else
            for {
              coefficient <- log1p.evaluate(Ratio.NegativeOne * difficultyCurve)
              result      <- exp.evaluate(coefficient * relativeStake)
            } yield Ratio.One - result
        }

        /**
         * Determines if the given proof meets the threshold to be elected slot leader
         * @param threshold the threshold to reach
         * @param rho the randomness
         * @return true if elected slot leader and false otherwise
         */
        def isSlotLeaderForThreshold(threshold: Ratio)(rho: Rho): F[Boolean] =
          blake2b512Resource.use(implicit blake2b512 =>
            Sync[F].delay {
              val testRhoHash = Ed25519VRF.rhoToRhoTestHash(rho)
              val testRhoHashBytes = testRhoHash.sizedBytes.data.toArray
              val test = Ratio(BigInt(Array(0x00.toByte) ++ testRhoHashBytes), NormalizationConstant, BigInt(1))
              (threshold > test)
            }
          )
      }

    def makeCached[F[_]: Sync: Clock](alg: LeaderElectionValidationAlgebra[F]): F[LeaderElectionValidationAlgebra[F]] =
      CaffeineCache[F, (Ratio, Slot), Ratio].map(cache =>
        new LeaderElectionValidationAlgebra[F] {

          override def getThreshold(relativeStake: Ratio, slotDiff: Slot): F[Ratio] =
            cache.cachingF((relativeStake, slotDiff))(ttl = None)(
              Sync[F].defer(
                alg.getThreshold(relativeStake, slotDiff)
              )
            )

          override def isSlotLeaderForThreshold(threshold: Ratio)(rho: Rho): F[Boolean] =
            alg.isSlotLeaderForThreshold(threshold)(rho)
        }
      )

  }
}
