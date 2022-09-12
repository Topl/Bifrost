package co.topl.demo

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl.{Flow, Keep, Source}
import akka.util.ByteString
import cats.data.{Store => _, _}
import cats.effect.implicits._
import cats.effect.kernel.Sync
import cats.effect.unsafe.{IORuntime, IORuntimeConfig}
import cats.effect.{Async, ExitCode, IO, IOApp}
import cats.implicits._
import co.topl.algebras._
import co.topl.blockchain.{BigBang, Blockchain, StakerInitializers}
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus._
import co.topl.consensus.interpreters._
import co.topl.crypto.hash.{Blake2b256, Blake2b512}
import co.topl.crypto.signing.{Curve25519, Ed25519, Ed25519VRF, ExtendedEd25519, KesProduct}
import co.topl.interpreters._
import co.topl.ledger.interpreters._
import co.topl.models._
import co.topl.models.utility.HasLength.instances.{bigIntLength, bytesLength}
import co.topl.models.utility.{Ratio, Sized}
import co.topl.networking.p2p.{DisconnectedPeer, LocalPeer, Locations, SimulatedGeospatialDelayFlow}
import co.topl.numerics.{ExpInterpreter, Log1pInterpreter}
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.net.InetSocketAddress
import java.nio.file.{Files, Paths}
import java.security.SecureRandom
import java.time.Instant
import scala.collection.immutable.ListSet
import scala.concurrent.duration._
import scala.util.Random

/**
 * Runs multiple independent DemoProgram instances, each with their own storage
 * and networking.  The instances share the same JVM, but they still communicate
 * over TCP and they each bind to an independent port on localhost.
 */
object TetraSuperDemo extends IOApp {

  import DemoConfig._
  import DemoUtils._

  // Actor system initialization

  implicit private val system: ActorSystem[_] =
    ActorSystem(Behaviors.empty, "TetraDemo")

  override val runtime: IORuntime = AkkaCatsRuntime(system).runtime
  override val runtimeConfig: IORuntimeConfig = AkkaCatsRuntime(system).ioRuntimeConfig

  // Interpreter initialization

  type F[A] = IO[A]

  private def makeClock(genesisTimestamp: Timestamp): ClockAlgebra[F] =
    SchedulerClock.Eval.make(SlotDuration, EpochLength, Instant.ofEpochMilli(genesisTimestamp))

  private val statsDir = Paths.get(".bifrost", "stats")
  Files.createDirectories(statsDir)

  private def statsInterpreter =
    StatsInterpreter.Eval.make[F](statsDir)

  // Program definition

  def run(args: List[String]): IO[ExitCode] = {
    for {
      bigBangTimestamp <- Instant.now().plusSeconds(10).toEpochMilli.pure[F]
      implicit0(networkPrefix: NetworkPrefix) = NetworkPrefix(1: Byte)
      configs = List(
        DemoNodeConfig("North1", Locations.NorthPole, 9090, 8090),
        DemoNodeConfig("North2", Locations.SouthPole, 9091, 8091, List("North1"))
      )
      implicit0(bigBangConfig: BigBang.Config) = BigBang.Config(
        bigBangTimestamp,
        Chain
          .fromSeq(configs.map(_.staker))
          .flatMap(_.bigBangOutputs(Sized.maxUnsafe(Ratio(TotalStake.data, configs.length).round)))
          .append(
            Transaction.Output(
              FullAddress(
                networkPrefix,
                Propositions.Contextual.HeightLock(1).spendingAddress,
                StakingAddresses.NonStaking,
                Proofs.Knowledge.Ed25519(Sized.strictUnsafe(Bytes.fill(64)(0: Byte)))
              ),
              Box.Values.Poly(10_000),
              minting = true
            )
          )
      )
      bigBangBlock = BigBang.block
      _ <- configs.parTraverse { demoNodeConfig =>
        runInstance(bigBangBlock)(
          demoNodeConfig.localPeer,
          Source(
            demoNodeConfig.outboundConnectionsTo
              .map(name => configs.find(_.name == name).get)
              .map(remoteConfig => DisconnectedPeer(remoteConfig.inetSocketAddress, remoteConfig.location))
          ),
          demoNodeConfig.staker,
          demoNodeConfig.name,
          configs.indexWhere(_.name == demoNodeConfig.name),
          demoNodeConfig.mintingEnabled,
          "localhost",
          demoNodeConfig.rpcPort
        )
      }
    } yield ()
  }
    .guarantee(
      Sync[F].delay(system.terminate()).flatMap(_ => Async[F].fromFuture(system.whenTerminated.pure[F])).void
    )
    .as(ExitCode.Success)

  private def runInstance(bigBangBlock: BlockV2.Full)(
    localPeer:                          LocalPeer,
    remotes:                            Source[DisconnectedPeer, _],
    staker:                             StakerInitializers.Operator,
    stakerName:                         String,
    stakerIndex:                        Int,
    mintingEnabled:                     Boolean,
    rpcHost:                            String,
    rpcPort:                            Int
  ) =
    Sync[F].defer(
      for {
        blake2b256Resource <- ActorPoolUnsafeResource.Eval.make[F, Blake2b256](new Blake2b256, _ => ())
        blake2b512Resource <- ActorPoolUnsafeResource.Eval.make[F, Blake2b512](new Blake2b512, _ => ())
        ed25519VRFResource <- ActorPoolUnsafeResource.Eval.make[F, Ed25519VRF](Ed25519VRF.precomputed(), _ => ())
        kesProductResource <- ActorPoolUnsafeResource.Eval.make[F, KesProduct](new KesProduct, _ => ())
        curve25519Resource <- ActorPoolUnsafeResource.Eval.make[F, Curve25519](new Curve25519, _ => ())
        ed25519Resource    <- ActorPoolUnsafeResource.Eval.make[F, Ed25519](new Ed25519, _ => ())
        extendedEd25519Resource <- ActorPoolUnsafeResource.Eval
          .make[F, ExtendedEd25519](ExtendedEd25519.precomputed(), _ => ())
        loggerColor = loggerColors(stakerIndex).toString
        implicit0(logger: Logger[F]) = Slf4jLogger
          .getLoggerFromName[F](s"node.${loggerColor}$stakerName${Console.RESET}")
          .withModifiedString(str => s"$loggerColor$str${Console.RESET}")
        _ <- Logger[F].info(
          show"Initializing node with genesis block id=${bigBangBlock.headerV2.id.asTypedBytes}" +
          show" and transactionIds=${bigBangBlock.transactions.map(_.id.asTypedBytes)}"
        )
        slotDataStore        <- RefStore.Eval.make[F, TypedIdentifier, SlotData]()
        blockHeaderStore     <- RefStore.Eval.make[F, TypedIdentifier, BlockHeaderV2]()
        blockBodyStore       <- RefStore.Eval.make[F, TypedIdentifier, BlockBodyV2]()
        transactionStore     <- RefStore.Eval.make[F, TypedIdentifier, Transaction]()
        boxStateStore        <- RefStore.Eval.make[F, TypedIdentifier, NonEmptySet[Short]]()
        epochBoundariesStore <- RefStore.Eval.make[F, Long, TypedIdentifier]()
        operatorStakesStore  <- RefStore.Eval.make[F, StakingAddresses.Operator, Int128]()
        totalStakesStore     <- RefStore.Eval.make[F, Unit, Int128]()
        registrationsStore   <- RefStore.Eval.make[F, StakingAddresses.Operator, Box.Values.Registrations.Operator]()
        _ <- slotDataStore.put(bigBangBlock.headerV2.id, bigBangBlock.headerV2.slotData(Ed25519VRF.precomputed()))
        _ <- blockHeaderStore.put(bigBangBlock.headerV2.id, bigBangBlock.headerV2)
        _ <- blockBodyStore.put(
          bigBangBlock.headerV2.id,
          ListSet.empty ++ bigBangBlock.transactions.map(_.id.asTypedBytes).toList
        )
        _ <- bigBangBlock.transactions.traverseTap(transaction => transactionStore.put(transaction.id, transaction))
        blockIdTree          <- BlockIdTree.make[F]
        _                    <- blockIdTree.associate(bigBangBlock.headerV2.id, bigBangBlock.headerV2.parentHeaderId)
        blockHeightTreeStore <- RefStore.Eval.make[F, Long, TypedIdentifier]()
        _                    <- blockHeightTreeStore.put(0, bigBangBlock.headerV2.parentHeaderId)
        _                    <- totalStakesStore.put((), 0)
        blockHeightTreeUnapplyStore <- RefStore.Eval.make[F, TypedIdentifier, Long]()
        blockHeightTree <- BlockHeightTree
          .make[F](
            blockHeightTreeStore,
            bigBangBlock.headerV2.parentHeaderId,
            slotDataStore,
            blockHeightTreeUnapplyStore,
            blockIdTree
          )
        clock = makeClock(bigBangBlock.headerV2.timestamp)
        etaCalculation <- EtaCalculation.Eval.make(
          slotDataStore.getOrRaise,
          clock,
          bigBangBlock.headerV2.eligibilityCertificate.eta,
          blake2b256Resource,
          blake2b512Resource
        )
        exp         <- ExpInterpreter.make[F](10000, 38)
        log1p       <- Log1pInterpreter.make[F](10000, 8)
        log1pCached <- Log1pInterpreter.makeCached[F](log1p)
        leaderElectionThreshold = LeaderElectionValidation.Eval.make[F](vrfConfig, blake2b512Resource, exp, log1pCached)
        epochBoundariesState <- EpochBoundariesEventSourcedState.make[F](
          clock,
          bigBangBlock.headerV2.parentHeaderId.pure[F],
          blockIdTree,
          epochBoundariesStore.pure[F],
          slotDataStore.getOrRaise
        )
        consensusDataState <- ConsensusDataEventSourcedState.make[F](
          bigBangBlock.headerV2.parentHeaderId.pure[F],
          blockIdTree,
          ConsensusDataEventSourcedState
            .ConsensusData(operatorStakesStore, totalStakesStore, registrationsStore)
            .pure[F],
          blockBodyStore.getOrRaise,
          transactionStore.getOrRaise,
          boxId =>
            transactionStore.getOrRaise(boxId.transactionId).map(_.outputs.get(boxId.transactionOutputIndex.toLong).get)
        )
        consensusValidationState <- ConsensusValidationState
          .make[F](bigBangBlock.headerV2.id, epochBoundariesState, consensusDataState, clock)
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
          bigBangBlock.headerV2.slotData(Ed25519VRF.precomputed()),
          ChainSelection
            .orderT[F](slotDataStore.getOrRaise, blake2b512Resource, ChainSelectionKLookback, ChainSelectionSWindow)
        )
        mempool <- Mempool.make[F](
          bigBangBlock.headerV2.parentHeaderId.pure[F],
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
          bigBangBlock.headerV2.parentHeaderId.pure[F],
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
        stakingOpt <- OptionT
          .whenF(mintingEnabled)(
            DemoUtils.createStaking[F](
              bigBangBlock.headerV2,
              staker,
              clock,
              etaCalculation,
              consensusValidationState,
              leaderElectionThreshold,
              ed25519Resource,
              ed25519VRFResource,
              kesProductResource
            )
          )
          .value
        _ <- Blockchain
          .run[F](
            clock,
            stakingOpt,
            slotDataStore,
            blockHeaderStore,
            blockBodyStore,
            transactionStore,
            localChain,
            blockIdTree,
            blockHeightTree,
            cachedHeaderValidation,
            transactionSyntaxValidation,
            bodySyntaxValidation,
            bodySemanticValidation,
            bodyAuthorizationValidation,
            mempool,
            ed25519VRFResource,
            localPeer,
            remotes.concat(Source.never),
            (peer, flow) => {
              val delayer =
                SimulatedGeospatialDelayFlow(
                  localPeer.coordinate,
                  peer.coordinate,
                  durationPerKilometer = 0.nanos, // 10.micros,
                  durationPerByte = 0.nanos, // 1.micros,
                  noise = 0.nanos // 30.milli
                )
              Flow[ByteString].via(delayer).viaMat(flow)(Keep.right).via(delayer)
            },
            rpcHost,
            rpcPort
          )
      } yield ()
    )

  private def parseAddress(host: String = "localhost", port: Int) =
    InetSocketAddress.createUnresolved(host, port)
}

case class DemoNodeConfig(
  name:                  String,
  location:              (Double, Double),
  p2pPort:               Int,
  rpcPort:               Int,
  outboundConnectionsTo: List[String] = Nil,
  mintingEnabled:        Boolean = true
) {

  val inetSocketAddress: InetSocketAddress =
    InetSocketAddress.createUnresolved("localhost", p2pPort)

  val localPeer: LocalPeer =
    LocalPeer(inetSocketAddress, location)

  val staker: StakerInitializers.Operator =
    StakerInitializers.Operator(Sized.strictUnsafe(Bytes(Random.nextBytes(32))), DemoConfig.KesKeyHeight)
}
