package co.topl.demo

import akka.util.Timeout
import cats.Parallel
import cats.effect.Async
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras._
import co.topl.blockchain.StakerInitializers
import co.topl.catsakka.FToFuture
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.LeaderElectionValidation.VrfConfig
import co.topl.consensus.algebras.{
  ConsensusValidationStateAlgebra,
  EtaCalculationAlgebra,
  LeaderElectionValidationAlgebra
}
import co.topl.crypto.signing.{Ed25519, Ed25519VRF, KesProduct}
import co.topl.interpreters.{InMemorySecureStore, StatsInterpreter}
import co.topl.minting._
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import java.util.UUID
import scala.concurrent.duration._

object DemoUtils {

  def createStaking[F[_]: Async: Parallel: FToFuture: Logger](
    bigBangHeader:            BlockHeaderV2,
    initializer:              StakerInitializers.Operator,
    clock:                    ClockAlgebra[F],
    etaCalculation:           EtaCalculationAlgebra[F],
    consensusValidationState: ConsensusValidationStateAlgebra[F],
    leaderElectionThreshold:  LeaderElectionValidationAlgebra[F],
    ed25519Resource:          UnsafeResource[F, Ed25519],
    ed25519VRFResource:       UnsafeResource[F, Ed25519VRF],
    kesProductResource:       UnsafeResource[F, KesProduct]
  ) =
    for {
      secureStore <- InMemorySecureStore.Eval.make[F]
      _           <- secureStore.write(UUID.randomUUID().toString, initializer.kesSK)
      vrfProofConstruction <- VrfProof.Eval.make[F](
        initializer.vrfSK,
        clock,
        leaderElectionThreshold,
        ed25519VRFResource,
        DemoConfig.vrfConfig
      )
      initialSlot  <- clock.globalSlot.map(_.max(0L))
      initialEpoch <- clock.epochOf(initialSlot)
      _            <- vrfProofConstruction.precomputeForEpoch(initialEpoch, bigBangHeader.eligibilityCertificate.eta)
      operationalKeys <- OperationalKeys.FromSecureStore.make[F](
        secureStore = secureStore,
        clock = clock,
        vrfProof = vrfProofConstruction,
        etaCalculation,
        consensusValidationState,
        kesProductResource,
        ed25519Resource,
        bigBangHeader.slotId,
        operationalPeriodLength = DemoConfig.OperationalPeriodLength,
        activationOperationalPeriod = 0L,
        initializer.stakingAddress,
        initialSlot = 0L
      )
      staking = Staking.Eval.make(
        initializer.stakingAddress,
        LeaderElectionMinting.Eval.make(
          initializer.vrfVK,
          leaderElectionThreshold,
          vrfProofConstruction,
          statsInterpreter = StatsInterpreter.Noop.make[F],
          statsName = ""
        ),
        operationalKeys,
        consensusValidationState,
        etaCalculation,
        ed25519Resource,
        vrfProofConstruction,
        clock
      )
    } yield staking

  val loggerColors = List(
    Console.MAGENTA,
    Console.BLUE,
    Console.YELLOW,
    Console.GREEN,
    Console.CYAN,
    Console.RED,
    Console.MAGENTA_B,
    Console.BLUE_B,
    Console.YELLOW_B,
    Console.GREEN_B,
    Console.CYAN_B,
    Console.RED_B
  )
}

object DemoConfig {

  val fEffective: Ratio = Ratio(15, 100)

  implicit val vrfConfig: VrfConfig =
    VrfConfig(lddCutoff = 15, precision = 40, baselineDifficulty = Ratio(1, 20), amplitude = Ratio(1, 2))

  val ChainSelectionKLookback: Long = 50
  val SlotDuration: FiniteDuration = 100.milli
  val OperationalPeriodsPerEpoch: Long = 2L

  val ChainSelectionSWindow: Long =
    (Ratio(ChainSelectionKLookback, 4L) * fEffective.inverse).round.toLong

  val EpochLength: Long =
    ChainSelectionSWindow * 6

  val OperationalPeriodLength: Long =
    EpochLength / OperationalPeriodsPerEpoch

  require(
    EpochLength % OperationalPeriodLength === 0L,
    "EpochLength must be evenly divisible by OperationalPeriodLength"
  )

  val KesKeyHeight: (Int, Int) =
    (9, 9)

  val OperatorRegistrationMaxLength: Long =
    OperationalPeriodLength * Ratio(2, 1).pow(KesKeyHeight._1 + KesKeyHeight._2).round.toLong

  val TotalStake: Int128 = 1_000_000L

  implicit val timeout: Timeout =
    Timeout(20.seconds)

}
