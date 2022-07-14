package co.topl.demo

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl.Source
import akka.util.Timeout
import cats.data.{Chain, NonEmptySet, OptionT}
import cats.effect.implicits._
import cats.effect.kernel.Sync
import cats.effect.unsafe.{IORuntime, IORuntimeConfig}
import cats.effect.{Async, ExitCode, IO, IOApp}
import cats.implicits._
import co.topl.algebras._
import co.topl.blockchain.Blockchain
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.LeaderElectionValidation.VrfConfig
import co.topl.consensus._
import co.topl.consensus.interpreters.ConsensusValidationState
import co.topl.crypto.hash.{Blake2b256, Blake2b512}
import co.topl.crypto.signing.{Curve25519, Ed25519, Ed25519VRF, ExtendedEd25519, KesProduct}
import co.topl.interpreters._
import co.topl.ledger.interpreters._
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility._
import co.topl.networking.p2p.{DisconnectedPeer, LocalPeer}
import co.topl.numerics.{ExpInterpreter, Log1pInterpreter}
import co.topl.typeclasses._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.net.InetSocketAddress
import java.nio.file.{Files, Paths}
import java.security.SecureRandom
import java.time.Instant
import java.util.UUID
import scala.concurrent.duration._
import scala.util.Random

/**
 * Command-line args:
 *
 * port rpcPort [remotes] seed stakerCount stakerIndex ?genesisTimestampMs
 * i.e.: 9094 8094 [localhost:9095] 2348921 10 3 1648049271191
 */
object TetraDemo extends IOApp {

  import DemoConfig._
  import DemoUtils._

  // Actor system initialization

  implicit private val system: ActorSystem[_] =
    ActorSystem(Behaviors.empty, "TetraDemo")

  override val runtime: IORuntime = AkkaCatsRuntime(system).runtime
  override val runtimeConfig: IORuntimeConfig = AkkaCatsRuntime(system).ioRuntimeConfig

  // Interpreter initialization

  type F[A] = IO[A]

  implicit private val logger: Logger[F] = Slf4jLogger.getLogger[F]

  private def makeClock(args: DemoArgs): ClockAlgebra[F] =
    SchedulerClock.Eval.make(SlotDuration, EpochLength, args.genesisTimestamp)

  private val statsDir = Paths.get(".bifrost", "stats")
  Files.createDirectories(statsDir)

  private val statsInterpreter =
    StatsInterpreter.Eval.make[F](statsDir)

  // Program definition

  def run(args: List[String]): IO[ExitCode] = {
    for {
      demoArgs <- DemoArgs.parse(args).pure[F]
      random = new Random(demoArgs.seed)
      stakers = computeStakers(demoArgs.stakerCount, random)
      blake2b256Resource <- ActorPoolUnsafeResource.Eval.make[F, Blake2b256](new Blake2b256, _ => ())
      blake2b512Resource <- ActorPoolUnsafeResource.Eval.make[F, Blake2b512](new Blake2b512, _ => ())
      ed25519VRFResource <- ActorPoolUnsafeResource.Eval.make[F, Ed25519VRF](Ed25519VRF.precomputed(), _ => ())
      kesProductResource <- ActorPoolUnsafeResource.Eval.make[F, KesProduct](new KesProduct, _ => ())
      curve25519Resource <- ActorPoolUnsafeResource.Eval.make[F, Curve25519](new Curve25519, _ => ())
      ed25519Resource    <- ActorPoolUnsafeResource.Eval.make[F, Ed25519](new Ed25519, _ => ())
      extendedEd25519Resource <- ActorPoolUnsafeResource.Eval
        .make[F, ExtendedEd25519](ExtendedEd25519.precomputed(), _ => ())
      slotDataStore    <- RefStore.Eval.make[F, TypedIdentifier, SlotData]()
      blockHeaderStore <- RefStore.Eval.make[F, TypedIdentifier, BlockHeaderV2]()
      blockBodyStore   <- RefStore.Eval.make[F, TypedIdentifier, BlockBodyV2]()
      transactionStore <- RefStore.Eval.make[F, TypedIdentifier, Transaction]()
      boxStateStore    <- RefStore.Eval.make[F, TypedIdentifier, NonEmptySet[Short]]()
      _                <- slotDataStore.put(genesis.headerV2.id, genesis.headerV2.slotData(Ed25519VRF.precomputed()))
      _                <- blockHeaderStore.put(genesis.headerV2.id, genesis.headerV2)
      _                <- blockBodyStore.put(genesis.headerV2.id, genesis.blockBodyV2)
      _                <- transactionStore.put(genesisTransaction.id, genesisTransaction)
      _                <- boxStateStore.put(genesisTransaction.id, NonEmptySet.one(0: Short))
      blockIdTree      <- BlockIdTree.make[F]
      _                <- blockIdTree.associate(genesis.headerV2.id, genesis.headerV2.parentHeaderId)
      blockHeightTreeStore        <- RefStore.Eval.make[F, Long, TypedIdentifier]()
      blockHeightTreeUnapplyStore <- RefStore.Eval.make[F, TypedIdentifier, Long]()
      blockHeightTree <- BlockHeightTree
        .make[F](
          blockHeightTreeStore,
          genesis.headerV2.parentHeaderId,
          slotDataStore,
          blockHeightTreeUnapplyStore,
          blockIdTree
        )
      clock = makeClock(demoArgs)
      etaCalculation <- EtaCalculation.Eval.make(
        slotDataStore.getOrRaise,
        clock,
        genesis.headerV2.eligibilityCertificate.eta,
        blake2b256Resource,
        blake2b512Resource
      )
      exp         <- ExpInterpreter.make[F](10000, 38)
      log1p       <- Log1pInterpreter.make[F](10000, 8)
      log1pCached <- Log1pInterpreter.makeCached[F](log1p)
      leaderElectionThreshold = LeaderElectionValidation.Eval.make[F](vrfConfig, blake2b512Resource, exp, log1pCached)
      consensusValidationState <- ConsensusValidationState.make[F](blockHeaderStore.getOrRaise, ???, ???, clock)
      underlyingHeaderValidation <- BlockHeaderValidation.Eval.make[F](
        etaCalculation,
        consensusValidationState,
        leaderElectionThreshold,
        ed25519VRFResource,
        kesProductResource,
        ed25519Resource,
        blake2b256Resource
      )
      cachedHeaderValidation <- BlockHeaderValidation.WithCache.make[F](underlyingHeaderValidation, blockHeaderStore)
      localChain <- LocalChain.Eval.make(
        genesis.headerV2.slotData(Ed25519VRF.precomputed()),
        ChainSelection
          .orderT[F](slotDataStore.getOrRaise, blake2b512Resource, ChainSelectionKLookback, ChainSelectionSWindow)
      )
      mempool <- Mempool.make[F](
        genesis.headerV2.id.asTypedBytes.pure[F],
        blockBodyStore.getOrRaise,
        transactionStore.getOrRaise,
        blockIdTree,
        clock,
        id => Logger[F].info(show"Expiring transaction id=$id"),
        Long.MaxValue,
        1000L
      )
      implicit0(networkRandom: Random) = new Random(new SecureRandom())
      boxState <- BoxState.make(
        genesis.headerV2.id.asTypedBytes.pure[F],
        blockBodyStore.getOrRaise,
        transactionStore.getOrRaise,
        blockIdTree,
        boxStateStore.pure[F]
      )
      transactionSyntaxValidation   <- TransactionSyntaxValidation.make[F]
      transactionSemanticValidation <- TransactionSemanticValidation.make[F](transactionStore.getOrRaise, boxState)
      transactionAuthorizationValidation <- TransactionAuthorizationValidation.make[F](
        blake2b256Resource,
        curve25519Resource,
        ed25519Resource,
        extendedEd25519Resource,
        slotDataStore.getOrRaise
      )
      bodySyntaxValidation <- BodySyntaxValidation.make[F](transactionStore.getOrRaise, transactionSyntaxValidation)
      bodySemanticValidation <- BodySemanticValidation.make[F](
        transactionStore.getOrRaise,
        boxState,
        boxState => TransactionSemanticValidation.make[F](transactionStore.getOrRaise, boxState)
      )
      bodyAuthorizationValidation <- BodyAuthorizationValidation.make[F](
        transactionStore.getOrRaise,
        transactionAuthorizationValidation
      )
      mintOpt <- OptionT
        .fromOption[F](demoArgs.stakerIndex)
        .semiflatMap(idx =>
          DemoUtils.createMint(
            genesis = genesis,
            staker = stakers(idx),
            clock = clock,
            etaCalculation = etaCalculation,
            leaderElectionThreshold = leaderElectionThreshold,
            localChain = localChain,
            mempool,
            blockHeaderStore,
            transactionStore.getOrRaise,
            bodySyntaxValidation,
            bodySemanticValidation,
            bodyAuthorizationValidation,
            consensusState,
            ed25519VRFResource,
            kesProductResource,
            ed25519Resource,
            statsInterpreter,
            OperationalPeriodLength
          )
        )
        .value
      _ <- Blockchain
        .run[F](
          mintOpt,
          slotDataStore,
          blockHeaderStore,
          blockBodyStore,
          transactionStore,
          localChain,
          blockIdTree,
          blockHeightTree,
          cachedHeaderValidation,
          transactionSyntaxValidation,
          transactionSemanticValidation,
          bodySyntaxValidation,
          bodySemanticValidation,
          bodyAuthorizationValidation,
          mempool,
          ed25519VRFResource,
          LocalPeer(InetSocketAddress.createUnresolved("localhost", demoArgs.port), (0, 0)),
          Source(demoArgs.remotes).delay(2.seconds).concat(Source.never),
          (_, flow) => flow,
          "localhost",
          demoArgs.rpcPort
        )
    } yield ()
  }
    .guarantee(
      Sync[F].delay(system.terminate()).flatMap(_ => Async[F].fromFuture(system.whenTerminated.pure[F])).void
    )
    .as(ExitCode.Success)
}

private case class Staker(
  relativeStake: Ratio,
  vrfKey:        SecretKeys.VrfEd25519,
  kesKey:        SecretKeys.KesProduct,
  registration:  Box.Values.Registrations.Operator,
  address:       StakingAddresses.Operator
)

case class DemoArgs(
  port:             Int,
  rpcPort:          Int,
  remotes:          List[DisconnectedPeer],
  seed:             Long,
  stakerCount:      Int,
  stakerIndex:      Option[Int],
  genesisTimestamp: Instant
)

object DemoArgs {

  def parse(args: List[String]): DemoArgs =
    DemoArgs(
      args(0).toInt,
      args(1).toInt,
      args(1)
        .drop(1)
        .dropRight(1)
        .split(',')
        .toList
        .filter(_.nonEmpty)
        .map(_.split(':'))
        .map(arr => DisconnectedPeer(InetSocketAddress.createUnresolved(arr(0), arr(1).toInt), (0, 0))),
      args(2).toLong,
      args(3).toInt,
      Option(args(4).toInt).filterNot(_ < 0),
      Instant.ofEpochMilli(args.lift(5).fold(defaultGenesisTimestamp())(a => a.toLong))
    )

  private def defaultGenesisTimestamp() = {
    val i = Instant.now()
    val t = i.toEpochMilli
    val result = t - (t % 10_000L) + 20_000L
    println(s"Initializing default genesisTimestamp=$result.  current=$t")
    result
  }
}
