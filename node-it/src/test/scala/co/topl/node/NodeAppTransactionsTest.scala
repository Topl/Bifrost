package co.topl.node

import cats.effect._
import cats.effect.implicits._
import cats.effect.std.{Random, SecureRandom}
import cats.implicits._
import co.topl.algebras.NodeRpc
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax._
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.models.BlockId
import co.topl.genus.services._
import co.topl.grpc.NodeGrpc
import co.topl.interpreters.NodeRpcOps.clientAsNodeRpcApi
import co.topl.node.Util._
import co.topl.transactiongenerator.interpreters.Fs2TransactionGenerator
import co.topl.typeclasses.implicits._
import fs2.io.file.{Files, Path}
import fs2.{io => _, _}
import munit._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration._

// scalastyle:off magic.number
class NodeAppTransactionsTest extends CatsEffectSuite {

  type F[A] = IO[A]

  type RpcClient = NodeRpc[F, Stream[F, *]]

  override val munitTimeout: Duration = 3.minutes

  def configNodeA(
    dataDir:                     Path,
    stakingDir:                  Path,
    genesisBlockId:              BlockId,
    genesisSourcePath:           String,
    rpcPort:                     Int,
    useMempoolForSemanticIfLess: Int = 100
  ): String =
    s"""
       |bifrost:
       |  data:
       |    directory: $dataDir
       |  staking:
       |    directory: $stakingDir
       |  p2p:
       |    bind-port: 9150
       |    public-port: 9150
       |  rpc:
       |    bind-port: $rpcPort
       |  big-bang:
       |    type: public
       |    genesis-id: ${genesisBlockId.show}
       |    source-path: $genesisSourcePath
       |  mempool:
       |    protection:
       |      enabled: true
       |      no-check-if-less: 0
       |      use-mempool-for-semantic-if-less: $useMempoolForSemanticIfLess
       |genus:
       |  enable: true
       |""".stripMargin

  def configNodeB(
    dataDir:                     Path,
    stakingDir:                  Path,
    genesisBlockId:              BlockId,
    genesisSourcePath:           String,
    rpcPort:                     Int,
    NodeAIp:                     String,
    useMempoolForSemanticIfLess: Int = 100
  ): String =
    s"""
       |bifrost:
       |  data:
       |    directory: $dataDir
       |  staking:
       |    directory: $stakingDir
       |  p2p:
       |    bind-port: 9152
       |    public-port: 9152
       |    known-peers: $NodeAIp:9150
       |  rpc:
       |    bind-port: $rpcPort
       |  big-bang:
       |    type: public
       |    genesis-id: ${genesisBlockId.show}
       |    source-path: $genesisSourcePath
       |  mempool:
       |    protection:
       |      enabled: true
       |      no-check-if-less: 0
       |      use-mempool-for-semantic-if-less: $useMempoolForSemanticIfLess
       |genus:
       |  enable: false
       |""".stripMargin

  test("Enabled memory pool protection, accept and broadcast transaction chain, tx in mempool is considered") {

    val height: Int = 3
    val rpcPortA: Int = 1951
    val rpcPortB: Int = 1953

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
            configNodeA(dataDir, stakingDir, testnetConfig.genesis.header.id, genesisSourcePath, rpcPortA)
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
            configNodeB(dataDir, stakingDir, testnetConfig.genesis.header.id, genesisSourcePath, rpcPortB, "127.0.0.2")
          }
          .flatMap(serveConfig(_, "nodeB.yaml"))
        // Run the nodes in separate fibers, but use the fibers' outcomes as an error signal to
        // the test by racing the computation
        _ <- (launch(configALocation), launch(configBLocation))
          .parMapN(_.race(_).map(_.merge).flatMap(_.embedNever))
          .flatMap(nodeCompletion =>
            nodeCompletion.toResource.race(for {
              rpcClientA <- NodeGrpc.Client.make[F]("127.0.0.2", rpcPortA, tls = false)
              rpcClientB <- NodeGrpc.Client.make[F]("127.0.0.3", rpcPortB, tls = false)
              rpcClients = List(rpcClientA, rpcClientB)
              implicit0(logger: Logger[F]) <- Slf4jLogger.fromName[F]("NodeAppTest").toResource
              _                            <- rpcClients.parTraverse(_.waitForRpcStartUp).toResource
              genusChannelA                <- co.topl.grpc.makeChannel[F]("127.0.0.2", rpcPortA, tls = false)
              genusTxServiceA              <- TransactionServiceFs2Grpc.stubResource[F](genusChannelA)
              genusBlockServiceA           <- BlockServiceFs2Grpc.stubResource[F](genusChannelA)
              _                            <- awaitGenusReady(genusBlockServiceA).timeout(45.seconds).toResource
              wallet                       <- makeWallet(genusTxServiceA)
              _                            <- IO(wallet.spendableBoxes.nonEmpty).assert.toResource
              implicit0(random: Random[F]) <- SecureRandom.javaSecuritySecureRandom[F].toResource

              transactionGenerator <-
                Fs2TransactionGenerator
                  .make[F](wallet, _ => 1000L.pure[F], Fs2TransactionGenerator.emptyMetadata[F])
                  .toResource
              transactionGraph <- Stream
                .force(transactionGenerator.generateTransactions)
                .take(10)
                .compile
                .toList
                .toResource
              _ <- IO(transactionGraph.sizeIs == 10).assert.toResource

              _ <- rpcClients.parTraverse(fetchUntilHeight(_, height)).toResource
              // Broadcast transactions to the node
              _ <- Stream
                .emits(transactionGraph)
                .covaryAll[F, IoTransaction]
                .evalMap(tx => rpcClientA.broadcastTransaction(tx))
                .compile
                .drain
                .toResource

              // Verify that the transactions were confirmed by both nodes
              _ <- rpcClients
                .parTraverse(client =>
                  Async[F].timeout(confirmTransactions(client)(transactionGraph.map(_.id).toSet), 60.seconds)
                )
                .toResource

            } yield ())
          )
      } yield ()
    resource.use_
  }

  test("Enabled memory pool protection, send tx chain to different nodes, thus only head tx will be accepted") {

    val height: Int = 3
    val rpcPortA: Int = 1961
    val rpcPortB: Int = 1963

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
            configNodeA(dataDir, stakingDir, testnetConfig.genesis.header.id, genesisSourcePath, rpcPortA)
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
            configNodeB(dataDir, stakingDir, testnetConfig.genesis.header.id, genesisSourcePath, rpcPortB, "127.0.0.4")
          }
          .flatMap(serveConfig(_, "nodeB.yaml"))
        // Run the nodes in separate fibers, but use the fibers' outcomes as an error signal to
        // the test by racing the computation
        _ <- (launch(configALocation), launch(configBLocation))
          .parMapN(_.race(_).map(_.merge).flatMap(_.embedNever))
          .flatMap(nodeCompletion =>
            nodeCompletion.toResource.race(for {
              rpcClientA <- NodeGrpc.Client.make[F]("127.0.0.4", rpcPortA, tls = false)
              rpcClientB <- NodeGrpc.Client.make[F]("127.0.0.5", rpcPortB, tls = false)
              rpcClients = List(rpcClientA, rpcClientB)
              implicit0(logger: Logger[F]) <- Slf4jLogger.fromName[F]("NodeAppTest").toResource
              _                            <- rpcClients.parTraverse(_.waitForRpcStartUp).toResource
              genusChannelA                <- co.topl.grpc.makeChannel[F]("127.0.0.4", rpcPortA, tls = false)
              genusTxServiceA              <- TransactionServiceFs2Grpc.stubResource[F](genusChannelA)
              genusBlockServiceA           <- BlockServiceFs2Grpc.stubResource[F](genusChannelA)
              _                            <- awaitGenusReady(genusBlockServiceA).timeout(45.seconds).toResource
              implicit0(random: Random[F]) <- SecureRandom.javaSecuritySecureRandom[F].toResource

              _ <- rpcClients.parTraverse(fetchUntilHeight(_, height)).toResource

              wallet <- makeWallet(genusTxServiceA)
              _      <- IO(wallet.spendableBoxes.nonEmpty).assert.toResource
              transactionGenerator <-
                Fs2TransactionGenerator
                  .make[F](wallet, _ => 1000L.pure[F], Fs2TransactionGenerator.emptyMetadata[F])
                  .toResource
              transactionGraph <- Stream
                .force(transactionGenerator.generateTransactions)
                .take(20)
                .compile
                .toList
                .toResource
              _ <- Logger[F].info(show"Generated txs: ${transactionGraph.map(_.id)}").toResource

              // send txs chain to different nodes,
              // most of them will be rejected because "parent" tx is sent to other node
              _ <-
                Stream
                  .repeatEval(random.elementOf(rpcClients))
                  .zip(Stream.evalSeq(random.shuffleList(transactionGraph)))
                  .evalMap { case (client, tx) => client.broadcastTransaction(tx) }
                  .compile
                  .drain
                  .toResource

              // verify that first transaction had been confirmed
              _ <- Async[F].timeout(fetchUntilTx(rpcClientB, transactionGraph.head.id), 30.seconds).toResource
              _ <- rpcClients
                .parTraverse(client =>
                  Async[F].timeout(confirmTransactions(client)(Set(transactionGraph.head.id)), 30.seconds)
                )
                .toResource

              // verify that last transaction had not been confirmed
              _ <- rpcClients
                .parTraverse(verifyNotConfirmed(_)(Set(transactionGraph.last.id)))
                .toResource

            } yield ())
          )
      } yield ()
    resource.use_
  }

  test("Enabled memory pool protection consider only first N transaction for semantic check") {
    val height: Int = 3
    val rpcPortA: Int = 1971
    val rpcPortB: Int = 1973

    val useMempoolForSemanticIfLess = 10

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
            configNodeA(
              dataDir,
              stakingDir,
              testnetConfig.genesis.header.id,
              genesisSourcePath,
              rpcPortA,
              useMempoolForSemanticIfLess
            )
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
            configNodeB(
              dataDir,
              stakingDir,
              testnetConfig.genesis.header.id,
              genesisSourcePath,
              rpcPortB,
              "127.0.0.6",
              useMempoolForSemanticIfLess
            )
          }
          .flatMap(serveConfig(_, "nodeB.yaml"))
        // Run the nodes in separate fibers, but use the fibers' outcomes as an error signal to
        // the test by racing the computation
        _ <- (launch(configALocation), launch(configBLocation))
          .parMapN(_.race(_).map(_.merge).flatMap(_.embedNever))
          .flatMap(nodeCompletion =>
            nodeCompletion.toResource.race(for {
              rpcClientA <- NodeGrpc.Client.make[F]("127.0.0.6", rpcPortA, tls = false)
              rpcClientB <- NodeGrpc.Client.make[F]("127.0.0.7", rpcPortB, tls = false)
              rpcClients = List(rpcClientA, rpcClientB)
              implicit0(logger: Logger[F]) <- Slf4jLogger.fromName[F]("NodeAppTest").toResource
              _                            <- rpcClients.parTraverse(_.waitForRpcStartUp).toResource
              genusChannelA                <- co.topl.grpc.makeChannel[F]("127.0.0.6", rpcPortA, tls = false)
              genusTxServiceA              <- TransactionServiceFs2Grpc.stubResource[F](genusChannelA)
              genusBlockServiceA           <- BlockServiceFs2Grpc.stubResource[F](genusChannelA)
              _                            <- awaitGenusReady(genusBlockServiceA).timeout(45.seconds).toResource

              _ <- rpcClients.parTraverse(fetchUntilHeight(_, height)).toResource

              wallet                       <- makeWallet(genusTxServiceA)
              _                            <- IO(wallet.spendableBoxes.nonEmpty).assert.toResource
              implicit0(random: Random[F]) <- SecureRandom.javaSecuritySecureRandom[F].toResource

              transactionGenerator <-
                Fs2TransactionGenerator
                  .make[F](wallet, _ => 1000L.pure[F], Fs2TransactionGenerator.emptyMetadata[F])
                  .toResource
              transactionGraph <- Stream
                .force(transactionGenerator.generateTransactions)
                .take(useMempoolForSemanticIfLess * 2)
                .compile
                .toList
                .toResource

              _ <- Logger[F].info(show"Generated txs: ${transactionGraph.map(_.id).zipWithIndex}").toResource
              _ <- IO(transactionGraph.sizeIs == useMempoolForSemanticIfLess * 2).assert.toResource
              _ <- rpcClients.parTraverse(fetchUntilHeight(_, height)).toResource
              // Broadcast _all_ of the good transactions to the nodes
              _ <- Stream
                .emits(transactionGraph)
                .covaryAll[F, IoTransaction]
                .evalMap(tx => rpcClientA.broadcastTransaction(tx))
                .compile
                .drain
                .toResource

              _ <- Async[F]
                .timeout(fetchUntilTx(rpcClientB, transactionGraph(useMempoolForSemanticIfLess - 1).id), 60.seconds)
                .toResource
              _ <- rpcClients
                .parTraverse(verifyNotConfirmed(_)(Set(transactionGraph.last.id)))
                .toResource
            } yield ())
          )
      } yield ()
    resource.use_
  }
}
// scalastyle:on magic.number
