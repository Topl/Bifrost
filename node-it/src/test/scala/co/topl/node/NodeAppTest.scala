package co.topl.node

import cats.data.OptionT
import cats.effect._
import cats.effect.implicits._
import cats.effect.std.{Random, SecureRandom}
import cats.implicits._
import co.topl.algebras.{NodeRpc, SynchronizationTraversalSteps}
import co.topl.blockchain.{BigBang, PrivateTestnet, StakerInitializers, StakingInit}
import co.topl.brambl.models._
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.{IoTransaction, UnspentTransactionOutput}
import co.topl.brambl.syntax._
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.config.ApplicationConfig
import co.topl.consensus.models.{BlockId, ProtocolVersion}
import co.topl.grpc.NodeGrpc
import co.topl.interpreters.NodeRpcOps.clientAsNodeRpcApi
import co.topl.models.utility._
import co.topl.node.models.{BlockBody, FullBlock}
import co.topl.transactiongenerator.interpreters.{Fs2TransactionGenerator, ToplRpcWalletInitializer}
import co.topl.typeclasses.implicits._
import com.comcast.ip4s.Port
import fs2._
import fs2.io.file.{Files, Path}
import munit._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.ember.server._
import org.http4s.server.Router
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import quivr.models.Int128

import java.nio.charset.StandardCharsets
import scala.concurrent.duration._

class NodeAppTest extends CatsEffectSuite {

  type F[A] = IO[A]

  type RpcClient = NodeRpc[F, Stream[F, *]]

  override val munitTimeout: Duration = 3.minutes

  test("Two block-producing nodes that maintain consensus") {
    // Allow the nodes to produce/adopt blocks until reaching this height
    val targetProductionHeight = 10
    // All of the nodes should agree on the same block at this height
    val targetConsensusHeight = 8
    def configNodeA(dataDir: Path, stakingDir: Path, genesisBlockId: BlockId, genesisSourcePath: String) =
      s"""
         |bifrost:
         |  data:
         |    directory: $dataDir
         |  staking:
         |    directory: $stakingDir
         |  p2p:
         |    bind-port: 9150
         |    public-port: 9150
         |    network-properties:
         |      legacy-network: false
         |  rpc:
         |    bind-port: 9151
         |  big-bang:
         |    type: public
         |    genesis-id: ${genesisBlockId.show}
         |    source-path: $genesisSourcePath
         |genus:
         |  enable: true
         |""".stripMargin
    def configNodeB(dataDir: Path, stakingDir: Path, genesisBlockId: BlockId, genesisSourcePath: String) =
      s"""
         |bifrost:
         |  data:
         |    directory: $dataDir
         |  staking:
         |    directory: $stakingDir
         |  p2p:
         |    bind-port: 9152
         |    public-port: 9152
         |    known-peers: localhost:9150
         |    network-properties:
         |      legacy-network: false
         |  rpc:
         |    bind-port: 9153
         |  big-bang:
         |    type: public
         |    genesis-id: ${genesisBlockId.show}
         |    source-path: $genesisSourcePath
         |genus:
         |  enable: false
         |""".stripMargin

    val resource =
      for {
        testnetConfig     <- createTestnetConfig.toResource
        genesisServerPort <- serveGenesisBlock(testnetConfig.genesis)
        genesisSourcePath = s"http://localhost:$genesisServerPort/${testnetConfig.genesis.header.id.show}"
        configALocation <- (
          Files.forAsync[F].tempDirectory,
          Files
            .forAsync[F]
            .tempDirectory
            .evalTap(saveStaker(_)(testnetConfig.stakers(0)._1, testnetConfig.stakers(0)._2))
        ).tupled
          .map { case (dataDir, stakingDir) =>
            configNodeA(dataDir, stakingDir, testnetConfig.genesis.header.id, genesisSourcePath)
          }
          .flatMap(saveLocalConfig(_, "nodeA"))
          .map(_.toString)
        configBLocation <- (
          Files.forAsync[F].tempDirectory,
          Files
            .forAsync[F]
            .tempDirectory
            .evalTap(saveStaker(_)(testnetConfig.stakers(1)._1, testnetConfig.stakers(1)._2))
        ).tupled
          .map { case (dataDir, stakingDir) =>
            configNodeB(dataDir, stakingDir, testnetConfig.genesis.header.id, genesisSourcePath)
          }
          .flatMap(serveConfig(_, "nodeB.yaml"))
        // Run the nodes in separate fibers, but use the fibers' outcomes as an error signal to
        // the test by racing the computation
        _ <- (launch(configALocation), launch(configBLocation))
          .mapN(_.race(_).map(_.merge).flatMap(_.embedNever))
          .flatMap(nodeCompletion =>
            nodeCompletion.toResource.race(for {
              rpcClientA <- NodeGrpc.Client.make[F]("localhost", 9151, tls = false)
              rpcClientB <- NodeGrpc.Client.make[F]("localhost", 9153, tls = false)
              rpcClients = List(rpcClientA, rpcClientB)
              implicit0(logger: Logger[F]) <- Slf4jLogger.fromName[F]("NodeAppTest").toResource
              _                            <- rpcClients.parTraverse(_.waitForRpcStartUp).toResource
              walletInitializer            <- ToplRpcWalletInitializer.make[F](rpcClientA, 1, 1).toResource
              wallet                       <- walletInitializer.initialize.toResource
              implicit0(random: Random[F]) <- SecureRandom.javaSecuritySecureRandom[F].toResource
              // Construct two competing graphs of transactions.
              // Graph 1 has higher fees and should be included in the chain
              transactionGenerator1 <- Fs2TransactionGenerator.make[F](wallet, 1, 1, feeF = _ => 1000).toResource
              transactionGraph1 <- Stream
                .force(transactionGenerator1.generateTransactions)
                .take(10)
                .compile
                .toList
                .toResource
              // Graph 2 has lower fees, so the Block Packer should never choose them
              transactionGenerator2 <- Fs2TransactionGenerator.make[F](wallet, 1, 1, feeF = _ => 10).toResource
              transactionGraph2 <- Stream
                .force(transactionGenerator2.generateTransactions)
                .take(10)
                .compile
                .toList
                .toResource
              _ <- rpcClients.parTraverse(fetchUntilHeight(_, 2)).toResource
              // Broadcast _all_ of the good transactions to the nodes randomly
              _ <-
                Stream
                  .repeatEval(random.elementOf(rpcClients))
                  .zip(Stream.evalSeq(random.shuffleList(transactionGraph1)))
                  .evalMap { case (client, tx) => client.broadcastTransaction(tx) }
                  .compile
                  .drain
                  .toResource
              // Verify that the good transactions were confirmed by both nodes
              _ <- rpcClients
                .parTraverse(client =>
                  Async[F].timeout(confirmTransactions(client)(transactionGraph1.map(_.id).toSet), 60.seconds)
                )
                .toResource
              // Submit the bad transactions
              _ <- Stream
                .repeatEval(random.elementOf(rpcClients))
                .zip(Stream.evalSeq(random.shuffleList(transactionGraph2)))
                .evalMap { case (client, tx) => client.broadcastTransaction(tx) }
                .compile
                .drain
                .toResource
              // Verify that the nodes are still making blocks properly
              _ <- rpcClients.parTraverse(fetchUntilHeight(_, targetProductionHeight)).toResource
              // Verify that the "bad" transactions did not make it onto the chain
              _ <- rpcClients
                .parTraverse(verifyNotConfirmed(_)(transactionGraph2.map(_.id).toSet))
                .toResource
              // Now check consensus
              idsAtTargetHeight <- rpcClients
                .traverse(client =>
                  OptionT(client.blockIdAtHeight(targetConsensusHeight)).getOrRaise(new IllegalStateException)
                )
                .toResource
              _ <- IO(idsAtTargetHeight.toSet.size == 1).assert.toResource
            } yield ())
          )
      } yield ()
    resource.use_
  }

  private def createTestnetConfig: F[TestnetConfig] =
    for {
      random <- SecureRandom.javaSecuritySecureRandom[F]
      protocol = PrivateTestnet.DefaultProtocol.copy(slotDuration = 500.milli)
      createStaker = random
        .nextBytes(32)
        .map(seed =>
          StakerInitializers
            .Operator(seed, (protocol.kesKeyHours, protocol.kesKeyHours), PrivateTestnet.HeightLockOneSpendingAddress)
        )
      staker0 <- createStaker
      staker1 <- createStaker
      unstakedTopl = UnspentTransactionOutput(
        PrivateTestnet.HeightLockOneSpendingAddress,
        Value.defaultInstance.withTopl(Value.TOPL(1_000_000L))
      )
      lvl = UnspentTransactionOutput(
        PrivateTestnet.HeightLockOneSpendingAddress,
        Value.defaultInstance.withLvl(Value.LVL(1_000_000L))
      )
      timestamp <- Async[F].realTime.map(_.plus(20.seconds).toMillis)
    } yield TestnetConfig(
      timestamp,
      List(staker0 -> 500_000L, staker1 -> 500_000L),
      List(unstakedTopl),
      List(lvl),
      protocol
    )

  /**
   * Launches an HTTP server which mimics the behavior of serving a genesis block from GitHub
   * @param genesis the block to serve
   * @return the localhost port number upon which the HTTP server is bound.  The port is randomly created based
   *         on host availability.
   */
  private def serveGenesisBlock(genesis: FullBlock): Resource[F, Int] =
    for {
      genesisId <- Sync[F].delay(genesis.header.id).toResource
      genesisIdStr = genesisId.show
      files = Map(
        s"$genesisIdStr.header.pbuf" -> genesis.header.toByteArray,
        s"$genesisIdStr.body.pbuf" -> BlockBody(
          genesis.fullBody.transactions.map(_.id),
          genesis.fullBody.rewardTransaction.map(_.id)
        ).toByteArray
      ) ++ genesis.fullBody.transactions.map(tx => s"${tx.id.show}.transaction.pbuf" -> tx.toByteArray)
      routes =
        HttpRoutes.of[F] { case GET -> Root / `genesisIdStr` / fileName =>
          files.get(fileName).fold(NotFound())(Ok(_))
        }
      server <- EmberServerBuilder
        .default[F]
        .withPort(Port.fromInt(0).get)
        .withHttpApp(Router("/" -> routes).orNotFound)
        .build
      port = server.address.getPort
    } yield port

  private def saveStaker(dir: Path)(staker: StakerInitializers.Operator, quantity: Int128) = {
    def saveFile(name: String, data: Array[Byte]) =
      Stream.chunk(Chunk.array(data)).through(Files.forAsync[F].writeAll(dir / name)).compile.drain

    saveFile(StakingInit.OperatorKeyName, staker.operatorSK.toByteArray) *>
    saveFile(StakingInit.VrfKeyName, staker.vrfSK.toByteArray) *>
    Files.forAsync[F].createDirectories(dir / StakingInit.KesDirectoryName) *>
    saveFile(
      s"${StakingInit.KesDirectoryName}/0",
      co.topl.codecs.bytes.tetra.instances.persistableKesProductSecretKey.persistedBytes(staker.kesSK).toByteArray
    ) *>
    saveFile(StakingInit.RegistrationTxName, staker.registrationTransaction(quantity).toByteArray)
  }

  /**
   * Serve a config file on a random port.  Returns the URL to access the file
   * @param config the contents to serve
   * @param name the file name
   * @return a URL to the file
   */
  private def serveConfig(config: String, name: String): Resource[F, String] =
    EmberServerBuilder
      .default[F]
      .withPort(Port.fromInt(0).get)
      .withHttpApp(Router("/" -> HttpRoutes.of[F] { case GET -> Root / `name` => Ok(config) }).orNotFound)
      .build
      .map(server => s"http://localhost:${server.address.getPort}/$name")

  private def saveLocalConfig(config: String, name: String) =
    for {
      file <- Files[F].tempFile(None, name, ".yaml", None)
      _ <- Stream
        .iterable(config.getBytes(StandardCharsets.UTF_8))
        .through(Files[F].writeAll(file))
        .compile
        .drain
        .toResource
    } yield file

  private def fetchUntilHeight(rpcClient: RpcClient, height: Long) =
    Stream
      .force(rpcClient.synchronizationTraversal())
      .collect { case SynchronizationTraversalSteps.Applied(id) => id }
      .evalMap(rpcClient.fetchBlockHeader)
      .map(_.get)
      .takeWhile(_.height <= height)
      .compile
      .drain

  private def launch(configLocation: String): Resource[F, F[Outcome[F, Throwable, Unit]]] =
    for {
      app1                      <- Sync[F].delay(new AbstractNodeApp {}).toResource
      (args, config, appConfig) <- app1.initialize(Array("--config", configLocation)).toResource
      backgroundOutcomeF        <- app1.run(args, config, appConfig).background
    } yield backgroundOutcomeF

  private def confirmTransactions(
    client: RpcClient
  )(ids: Set[TransactionId], confirmationDepth: Int = 3): F[Unit] = {
    def filterTransactions(targetBlock: BlockId)(ids: Set[TransactionId]): F[Set[TransactionId]] =
      client
        .fetchBlockBody(targetBlock)
        .map(ids -- _.get.allTransactionIds)
        .flatMap(ids =>
          if (ids.isEmpty) ids.pure[F]
          else
            client
              .fetchBlockHeader(targetBlock)
              .map(_.get)
              .flatMap(header =>
                if (header.height > 1) filterTransactions(header.parentHeaderId)(ids)
                else ids.pure[F]
              )
        )

    Stream
      .retry(
        client
          .blockIdAtDepth(confirmationDepth)
          .iterateUntil(_.nonEmpty)
          .map(_.get)
          .flatMap(filterTransactions(_)(ids))
          .map(_.isEmpty)
          .assert,
        1500.milli,
        identity,
        30
      )
      .compile
      .drain
  }

  private def verifyNotConfirmed(client: RpcClient)(ids: Set[TransactionId]) =
    client.history
      .flatMap(block => Stream.emits(block.fullBody.allTransactions))
      .map(_.id)
      .forall(!ids.contains(_))
      .compile
      .lastOrError
      .assert

}

case class TestnetConfig(
  timestamp:     Long,
  stakers:       List[(StakerInitializers.Operator, Int128)],
  unstakedTopls: List[UnspentTransactionOutput],
  lvls:          List[UnspentTransactionOutput],
  protocol:      ApplicationConfig.Bifrost.Protocol
) {

  val protocolUtxo: UnspentTransactionOutput =
    UnspentTransactionOutput(PrivateTestnet.HeightLockOneSpendingAddress, BigBang.protocolToUpdateProposal(protocol))

  val genesisTransactions: List[IoTransaction] =
    stakers.map { case (init, quantity) => init.registrationTransaction(quantity) } :+ IoTransaction.defaultInstance
      .withOutputs(unstakedTopls ++ lvls :+ protocolUtxo)

  val genesis: FullBlock =
    BigBang.fromConfig(
      BigBang.Config(
        timestamp,
        genesisTransactions,
        protocolVersion = ProtocolVersion(2, 0, 0)
      )
    )
}
