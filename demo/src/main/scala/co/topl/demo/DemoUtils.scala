package co.topl.demo

import akka.actor.typed.ActorSystem
import akka.util.Timeout
import cats.Parallel
import cats.data.Chain
import cats.effect.Async
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras._
import co.topl.catsakka.FToFuture
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.LeaderElectionValidation.VrfConfig
import co.topl.consensus.algebras.{ConsensusValidationStateAlgebra, EtaCalculationAlgebra, LeaderElectionValidationAlgebra, LocalChainAlgebra}
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.signing.{Ed25519, Ed25519VRF, KesProduct}
import co.topl.interpreters.{AkkaSecureStore, StatsInterpreter}
import co.topl.ledger.algebras.{BodyAuthorizationValidationAlgebra, BodySemanticValidationAlgebra, BodySyntaxValidationAlgebra, MempoolAlgebra}
import co.topl.minting.algebras.PerpetualBlockMintAlgebra
import co.topl.minting._
import co.topl.models.utility.{Lengths, Ratio, Sized}
import co.topl.models._
import co.topl.models.utility.HasLength.instances.{bigIntLength, bytesLength}
import co.topl.typeclasses.BlockGenesis
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import java.nio.file.Files
import java.util.UUID
import scala.collection.immutable.ListMap
import scala.concurrent.duration._
import scala.util.Random

object DemoUtils {

  def createMint[F[_]: Async: Parallel: FToFuture](
    genesis:                     BlockV2,
    staker:                      Staker,
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
      _            <- Logger[F].info(show"Initializing staker key idx=0 address=${staker.address}")
      stakerKeyDir <- Async[F].blocking(Files.createTempDirectory(show"TetraDemoStaker${staker.address}"))
      secureStore  <- AkkaSecureStore.Eval.make[F](stakerKeyDir)
      _            <- secureStore.write(UUID.randomUUID().toString, staker.kesKey)
      vrfProofConstruction <- VrfProof.Eval.make[F](
        staker.vrfKey,
        clock,
        leaderElectionThreshold,
        ed25519VRFResource,
        vrfConfig
      )
      initialSlot  <- clock.globalSlot.map(_.max(0L))
      initialEpoch <- clock.epochOf(initialSlot)
      _            <- vrfProofConstruction.precomputeForEpoch(initialEpoch, genesis.headerV2.eligibilityCertificate.eta)
      operationalKeys <- OperationalKeys.FromSecureStore.make[F](
        secureStore = secureStore,
        clock = clock,
        vrfProof = vrfProofConstruction,
        etaCalculation,
        consensusState,
        kesProductResource,
        ed25519Resource,
        genesis.headerV2.slotId,
        operationalPeriodLength = operationalPeriodLength,
        activationOperationalPeriod = 0L,
        staker.address,
        initialSlot = initialSlot,
        genesis.headerV2.id
      )
      stakerVRFVK <- ed25519VRFResource.use(_.getVerificationKey(staker.vrfKey).pure[F])
      mint =
        BlockMint.Eval.make(
          Staking.Eval.make(
            staker.address,
            LeaderElectionMinting.Eval.make(
              stakerVRFVK,
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

  def computeStakers(count: Int, random: Random) = List.tabulate(count) { idx =>
    implicit val ed25519Vrf: Ed25519VRF = Ed25519VRF.precomputed()
    implicit val kesProduct: KesProduct = new KesProduct
    val seed = Sized.strictUnsafe[Bytes, Lengths.`32`.type](Bytes(random.nextBytes(32)))

    val (_, poolVK) = new Ed25519().deriveKeyPairFromSeed(seed)

    val (stakerVrfKey, _) =
      ed25519Vrf.deriveKeyPairFromSeed(seed)

    val (kesKey, _) =
      kesProduct.createKeyPair(seed = seed.data, height = DemoConfig.KesKeyHeight, 0)

    val stakerRegistration: Box.Values.Registrations.Operator =
      Box.Values.Registrations.Operator(
        vrfCommitment = kesProduct.sign(
          kesKey,
          new Blake2b256().hash(ed25519Vrf.getVerificationKey(stakerVrfKey).immutableBytes, poolVK.bytes.data).data
        )
      )

    Staker(Ratio(1, count), stakerVrfKey, kesKey, stakerRegistration, StakingAddresses.Operator(poolVK))
  }
}

object DemoConfig {

  val fEffective: Ratio = Ratio(15, 100)

  implicit val vrfConfig: VrfConfig =
    VrfConfig(lddCutoff = 15, precision = 40, baselineDifficulty = Ratio(1, 20), amplitude = Ratio(1, 2))

  val ChainSelectionKLookback: Long = 50
  val SlotDuration: FiniteDuration = 100.milli
  val OperationalPeriodsPerEpoch: Long = 2L

  val ChainSelectionSWindow: Long =
    (Ratio(ChainSelectionKLookback, 4) * fEffective.inverse).toDouble.ceil.round

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

  def genesisTransaction(outputs: Chain[Transaction.Output]): Transaction =
    Transaction(
      inputs = Chain.empty,
      outputs = outputs,
      chronology = Transaction.Chronology(0L, 0L, Long.MaxValue),
      None
    )

  val genesis: BlockV2 =
    BlockGenesis(List(genesisTransaction)).value

  implicit val timeout: Timeout =
    Timeout(20.seconds)

}
