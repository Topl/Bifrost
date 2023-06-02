package co.topl.node

import cats.Applicative
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
import co.topl.grpc.NodeGrpc
import co.topl.transactiongenerator.interpreters.Fs2TransactionGenerator
import co.topl.transactiongenerator.interpreters.ToplRpcWalletInitializer
import co.topl.typeclasses.implicits._
import fs2._
import fs2.io.file.Files
import fs2.io.file.Path
import munit._

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
    val startTimestamp = System.currentTimeMillis() + 10_000L
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
         |    experimental: false
         |  rpc:
         |    bind-port: 9151
         |  big-bang:
         |    staker-count: 2
         |    timestamp: $startTimestamp
         |  protocols:
         |    0:
         |      slot-duration: 500 milli
         |genus:
         |  enable: false
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
         |    experimental: false
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
        configFileA       <- saveLocalConfig(configNodeA, "nodeA")
        configFileB       <- saveLocalConfig(configNodeB, "nodeB")
        _                 <- launch(configFileA)
        _                 <- launch(configFileB)
        rpcClientA        <- NodeGrpc.Client.make[F]("localhost", 9151, tls = false)
        rpcClientB        <- NodeGrpc.Client.make[F]("localhost", 9153, tls = false)
        _                 <- (awaitNodeReady(rpcClientA).toResource, awaitNodeReady(rpcClientB).toResource).parTupled
        walletInitializer <- ToplRpcWalletInitializer.make[F](rpcClientA, 1, 1).toResource
        wallet            <- walletInitializer.initialize.toResource
        implicit0(random: Random[F]) <- SecureRandom.javaSecuritySecureRandom[F].toResource
        // Construct two competing graphs of transactions.
        // Graph 1 has higher fees and should be included in the chain
        transactionGenerator1 <- Fs2TransactionGenerator.make[F](wallet, 1, 1, feeF = _ => 1000).toResource
        transactionGraph1 <- Stream.force(transactionGenerator1.generateTransactions).take(10).compile.toList.toResource
        // Graph 2 has lower fees, so the Block Packer should never choose them
        transactionGenerator2 <- Fs2TransactionGenerator.make[F](wallet, 1, 1, feeF = _ => 10).toResource
        transactionGraph2 <- Stream.force(transactionGenerator2.generateTransactions).take(10).compile.toList.toResource
        _ <- (
          fetchUntilHeight(rpcClientA, 2).toResource,
          fetchUntilHeight(rpcClientB, 2).toResource
        ).parTupled
        _ <-
          Stream
            .repeatEval(random.elementOf(List(rpcClientA, rpcClientB)))
            .zip(Stream.evalSeq(random.shuffleList(transactionGraph1 ++ transactionGraph2)))
            .evalMap { case (client, tx) => client.broadcastTransaction(tx) }
            .compile
            .drain
            .toResource
        _ <- transactionGraph1
          .map(_.id)
          .parTraverse(tx => Async[F].timeout(confirmTransaction(rpcClientA)(tx.id), 60.seconds))
          .toResource
        _ <- (
          fetchUntilHeight(rpcClientA, targetProductionHeight).toResource,
          fetchUntilHeight(rpcClientB, targetProductionHeight).toResource
        ).parTupled
        (idA, idB) <- (
          OptionT(rpcClientA.blockIdAtHeight(targetConsensusHeight)).getOrRaise(new IllegalStateException).toResource,
          OptionT(rpcClientB.blockIdAtHeight(targetConsensusHeight)).getOrRaise(new IllegalStateException).toResource
        ).parTupled
        _ <- IO(idA === idB).assert.toResource
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
      _ <- Sync[F]
        .delay(
          app1.initialize(Array("--config", configFile.toString))
        )
        .toResource
      bg <- app1.run.start.toResource
      _  <- Resource.onFinalize(bg.cancel)
    } yield ()

  private def awaitNodeReady(client: NodeRpc[F, Stream[F, *]]) =
    Stream
      .retry(
        client
          .blockIdAtHeight(1)
          .map(_.get)
          .flatMap(client.fetchBlockHeader)
          .map(_.get.timestamp)
          .flatMap(bigBangTimestamp => Async[F].realTimeInstant.map(bigBangTimestamp - _.toEpochMilli).map(_.milli))
          .flatMap(durationUntilBigBang =>
            Applicative[F].whenA(durationUntilBigBang.toMillis > 0)(Async[F].sleep(durationUntilBigBang))
          ),
        250.milli,
        identity,
        200
      )
      .compile
      .drain

  private def confirmTransaction(
    client: RpcClient
  )(id: TransactionId, confirmationDepth: Int = 3): F[Unit] = {
    def containsTransaction(targetBlock: BlockId): F[Boolean] =
      client
        .fetchBlockBody(targetBlock)
        .map(_.get.transactionIds.contains(id))
        .ifM(
          true.pure[F],
          client
            .fetchBlockHeader(targetBlock)
            .map(_.get)
            .flatMap(header =>
              if (header.height > 1) containsTransaction(header.parentHeaderId)
              else false.pure[F]
            )
        )

    client
      .blockIdAtDepth(confirmationDepth)
      .iterateUntil(_.nonEmpty)
      .map(_.get)
      .flatMap(containsTransaction)
      .flatTap(if (_) Async[F].unit else Async[F].delayBy(Async[F].unit, 100.milli))
      .iterateUntil(identity)
      .void
  }
}
