package co.topl.demo

import akka.actor.typed.ActorSystem
import akka.util.Timeout
import cats.Parallel
import cats.effect.Async
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras._
import co.topl.blockchain.StakerInitializers
import co.topl.catsakka.FToFuture
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.LeaderElectionValidation.VrfConfig
import co.topl.consensus.algebras.{
  ConsensusValidationStateAlgebra,
  EtaCalculationAlgebra,
  LeaderElectionValidationAlgebra,
  LocalChainAlgebra
}
import co.topl.crypto.signing.{Ed25519, Ed25519VRF, KesProduct}
import co.topl.interpreters.{AkkaSecureStore, StatsInterpreter}
import co.topl.ledger.algebras.{
  BodyAuthorizationValidationAlgebra,
  BodySemanticValidationAlgebra,
  BodySyntaxValidationAlgebra,
  MempoolAlgebra
}
import co.topl.minting._
import co.topl.minting.algebras.PerpetualBlockMintAlgebra
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Ratio, Sized}
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import java.nio.file.Files
import java.util.UUID
import scala.concurrent.duration._
import scala.util.Random

object DemoUtils {

  def createMint[F[_]: Async: Parallel: FToFuture](
    bigBangHeader:               BlockHeaderV2,
    staker:                      StakerInitializers.Operator,
    clock:                       ClockAlgebra[F],
    etaCalculation:              EtaCalculationAlgebra[F],
    consensusState:              ConsensusValidationStateAlgebra[F],
    leaderElectionThreshold:     LeaderElectionValidationAlgebra[F],
    localChain:                  LocalChainAlgebra[F],
    mempool:                     MempoolAlgebra[F],
    headerStore:                 Store[F, TypedIdentifier, BlockHeaderV2],
    fetchTransaction:            TypedIdentifier => F[Transaction],
    bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:      BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F],
    ed25519VRFResource:          UnsafeResource[F, Ed25519VRF],
    kesProductResource:          UnsafeResource[F, KesProduct],
    ed25519Resource:             UnsafeResource[F, Ed25519],
    statsInterpreter:            Stats[F],
    operationalPeriodLength:     Long
  )(implicit
    logger:    Logger[F],
    system:    ActorSystem[_],
    timeout:   Timeout,
    vrfConfig: VrfConfig
  ): F[PerpetualBlockMintAlgebra[F]] =
    for {
      _            <- Logger[F].info(show"Initializing staker key idx=0 address=${staker.stakingAddress}")
      stakerKeyDir <- Async[F].blocking(Files.createTempDirectory(show"TetraDemoStaker${staker.stakingAddress}"))
      secureStore  <- AkkaSecureStore.Eval.make[F](stakerKeyDir)
      _            <- secureStore.write(UUID.randomUUID().toString, staker.kesSK)
      vrfProofConstruction <- VrfProof.Eval.make[F](
        staker.vrfSK,
        clock,
        leaderElectionThreshold,
        ed25519VRFResource,
        vrfConfig
      )
      initialSlot  <- clock.globalSlot.map(_.max(0L))
      initialEpoch <- clock.epochOf(initialSlot)
      _            <- vrfProofConstruction.precomputeForEpoch(initialEpoch, bigBangHeader.eligibilityCertificate.eta)
      operationalKeys <- OperationalKeys.FromSecureStore.make[F](
        secureStore = secureStore,
        clock = clock,
        vrfProof = vrfProofConstruction,
        etaCalculation,
        consensusState,
        kesProductResource,
        ed25519Resource,
        bigBangHeader.slotId,
        operationalPeriodLength = operationalPeriodLength,
        activationOperationalPeriod = 0L,
        staker.stakingAddress,
        initialSlot = initialSlot
      )
      mint =
        BlockMint.Eval.make(
          Staking.Eval.make(
            staker.stakingAddress,
            LeaderElectionMinting.Eval.make(
              staker.vrfVK,
              leaderElectionThreshold,
              vrfProofConstruction,
              statsInterpreter = StatsInterpreter.Noop.make[F],
              statsName = ""
            ),
            operationalKeys,
            consensusState,
            etaCalculation,
            ed25519Resource,
            vrfProofConstruction,
            clock
          ),
          clock,
          statsInterpreter
        )
      perpetual <- PerpetualBlockMint.InAkkaStream
        .make(
          clock,
          mint,
          localChain,
          mempool,
          headerStore,
          fetchTransaction,
          bodySyntaxValidation,
          bodySemanticValidation,
          bodyAuthorizationValidation
        )
    } yield perpetual

  def computeStakers(count: Int, random: Random): List[StakerInitializers.Operator] =
    List.fill(count) {
      StakerInitializers.Operator(Sized.strictUnsafe(Bytes(random.nextBytes(32))), DemoConfig.KesKeyHeight)
    }

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
