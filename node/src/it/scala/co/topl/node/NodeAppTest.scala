package co.topl.node

import cats.Applicative
import cats.data.OptionT
import cats.effect._
import cats.implicits._
import cats.effect.implicits._
import co.topl.algebras.{SynchronizationTraversalSteps, ToplRpc}
import co.topl.blockchain.PrivateTestnet
import co.topl.brambl.common.ContainsSignable.ContainsSignableTOps
import co.topl.brambl.common.ContainsSignable.instances.ioTransactionSignable
import co.topl.brambl.models._
import co.topl.brambl.models.box.Attestation
import co.topl.brambl.models.transaction._
import co.topl.brambl.syntax._
import co.topl.consensus.models.BlockId
import co.topl.grpc.ToplGrpc
import co.topl.quivr.api.Prover
import co.topl.typeclasses.implicits._
import fs2._
import fs2.io.file.{Files, Path}
import munit._
import quivr.models.SmallData

import java.nio.charset.StandardCharsets
import scala.concurrent.duration._

class NodeAppTest extends CatsEffectSuite {

  type F[A] = IO[A]

  type RpcClient = ToplRpc[F, Stream[F, *]]

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
        |    experimental: true
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
        |  rpc-node-port: 9151
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
         |    experimental: true
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
        rpcClientA  <- ToplGrpc.Client.make[F]("localhost", 9151, tls = false)
        rpcClientB  <- ToplGrpc.Client.make[F]("localhost", 9153, tls = false)
        _           <- (awaitNodeReady(rpcClientA).toResource, awaitNodeReady(rpcClientB).toResource).parTupled
        _ <- (
          fetchUntilHeight(rpcClientA, 3).toResource,
          fetchUntilHeight(rpcClientB, 3).toResource
        ).parTupled
        (utxoAddress, utxo) <- bigBangSpendableUtxo(rpcClientA).toResource
        newTransaction      <- spendUtxo(rpcClientB)(utxoAddress, utxo).toResource
        _ <- (
          Async[F].timeout(confirmTransaction(rpcClientA)(newTransaction.id), 60.seconds).toResource,
          Async[F].timeout(confirmTransaction(rpcClientB)(newTransaction.id), 60.seconds).toResource
        ).parTupled
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

  private def awaitNodeReady(client: ToplRpc[F, Stream[F, *]]) =
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

  private def bigBangSpendableUtxo(client: RpcClient): F[(TransactionOutputAddress, UnspentTransactionOutput)] =
    client
      .blockIdAtHeight(1)
      .map(_.get)
      .flatMap(client.fetchBlockBody)
      .map(_.get)
      .map(_.transactionIds.head)
      .flatMap(client.fetchTransaction)
      .map(_.get)
      .map { t =>
        val index = t.outputs.indexWhere(_.address == PrivateTestnet.HeightLockOneSpendingAddress)
        val address = t.id.outputAddress(0, 0, index)
        (address, t.outputs(index))
      }

  private def spendUtxo(
    client: RpcClient
  )(address: TransactionOutputAddress, output: UnspentTransactionOutput) =
    for {
      predicate <- Attestation.Predicate(PrivateTestnet.HeightLockOneLock.getPredicate, Nil).pure[F]
      unprovenTransaction <- IoTransaction.defaultInstance
        .withInputs(
          List(
            SpentTransactionOutput(
              address,
              Attestation(Attestation.Value.Predicate(predicate)),
              output.value
            )
          )
        )
        .withOutputs(
          List(
            UnspentTransactionOutput(
              PrivateTestnet.HeightLockOneSpendingAddress,
              output.value
            )
          )
        )
        .withDatum(
          Datum.IoTransaction(
            Event.IoTransaction(Schedule(0, Long.MaxValue, System.currentTimeMillis()), SmallData.defaultInstance)
          )
        )
        .pure[F]
      proof <- Prover.heightProver[F].prove((), unprovenTransaction.signable)
      provenTransaction = unprovenTransaction.copy(
        inputs = unprovenTransaction.inputs.map(
          _.copy(attestation =
            Attestation(
              Attestation.Value.Predicate(
                predicate.copy(responses = List(proof))
              )
            )
          )
        )
      )
      _ <- client.broadcastTransaction(provenTransaction)
    } yield provenTransaction

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
      .iterateUntil(identity)
      .void
  }
}
