package co.topl.node

import cats.data.OptionT
import cats.effect._
import cats.effect.implicits._
import cats.effect.std.Random
import cats.effect.std.SecureRandom
import cats.implicits._
import co.topl.algebras.NodeRpc
import co.topl.algebras.SynchronizationTraversalSteps
import co.topl.brambl.models._
import co.topl.brambl.syntax._
import co.topl.consensus.models.BlockId
import co.topl.models.utility._
import co.topl.grpc.NodeGrpc
import co.topl.interpreters.NodeRpcOps.clientAsNodeRpcApi
import co.topl.transactiongenerator.interpreters.Fs2TransactionGenerator
import co.topl.transactiongenerator.interpreters.ToplRpcWalletInitializer
import fs2._
import fs2.io.file.Files
import fs2.io.file.Path
import munit._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

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
    val startTimestamp = System.currentTimeMillis() + 20_000L
    val configNodeA =
      s"""
         |bifrost:
         |  data:
         |    directory: /tmp/bifrostNodeA/data
         |  staking:
         |    directory: /tmp/bifrostNodeA/staking
         |  p2p:
         |    bind-port: 9150
         |    public-port: 9150
         |    network-properties:
         |      legacy-network: false
         |  rpc:
         |    bind-port: 9151
         |  big-bang:
         |    staker-count: 2
         |    timestamp: $startTimestamp
         |  protocols:
         |    0:
         |      slot-duration: 500 milli
         |genus:
         |  enable: true
         |""".stripMargin
    val configNodeB =
      s"""
         |bifrost:
         |  data:
         |    directory: /tmp/bifrostNodeB/data
         |  staking:
         |    directory: /tmp/bifrostNodeB/staking
         |  p2p:
         |    bind-port: 9152
         |    public-port: 9152
         |    known-peers: localhost:9150
         |    network-properties:
         |      legacy-network: false
         |  rpc:
         |    bind-port: 9153
         |  big-bang:
         |    staker-count: 2
         |    local-staker-index: 1
         |    timestamp: $startTimestamp
         |  protocols:
         |    0:
         |      slot-duration: 500 milli
         |genus:
         |  enable: false
         |""".stripMargin

    val resource =
      for {
        configFileA <- saveLocalConfig(configNodeA, "nodeA")
        configFileB <- saveLocalConfig(configNodeB, "nodeB")
        _           <- launch(configFileA)
        _           <- launch(configFileB)
        rpcClientA  <- NodeGrpc.Client.make[F]("localhost", 9151, tls = false)
        rpcClientB  <- NodeGrpc.Client.make[F]("localhost", 9153, tls = false)
        rpcClients = List(rpcClientA, rpcClientB)
        implicit0(logger: Logger[F]) <- Slf4jLogger.fromName[F]("NodeAppTest").toResource
        _                            <- rpcClients.parTraverse(_.waitForRpcStartUp).toResource
        walletInitializer            <- ToplRpcWalletInitializer.make[F](rpcClientA, 1, 1).toResource
        wallet                       <- walletInitializer.initialize.toResource
        implicit0(random: Random[F]) <- SecureRandom.javaSecuritySecureRandom[F].toResource
        // Construct two competing graphs of transactions.
        // Graph 1 has higher fees and should be included in the chain
        transactionGenerator1 <- Fs2TransactionGenerator.make[F](wallet, 1, 1, feeF = _ => 1000).toResource
        transactionGraph1 <- Stream.force(transactionGenerator1.generateTransactions).take(10).compile.toList.toResource
        // Graph 2 has lower fees, so the Block Packer should never choose them
        transactionGenerator2 <- Fs2TransactionGenerator.make[F](wallet, 1, 1, feeF = _ => 10).toResource
        transactionGraph2 <- Stream.force(transactionGenerator2.generateTransactions).take(10).compile.toList.toResource
        _                 <- rpcClients.parTraverse(fetchUntilHeight(_, 2)).toResource
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
      } yield ()
    resource.use_
  }

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

  private def launch(configFile: Path): Resource[F, Unit] =
    for {
      app1 <- Sync[F].delay(new AbstractNodeApp {}).toResource
      _    <- Sync[F].delay(app1.initialize(Array("--config", configFile.toString))).toResource
      bg   <- app1.run.start.toResource
      _    <- Resource.onFinalize(bg.cancel)
    } yield ()

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
