package co.topl.testnetsimulationorchestrator.app

import cats.data.OptionT
import cats.effect._
import cats.implicits._
import co.topl.algebras.{SynchronizationTraversalSteps, ToplRpc}
import co.topl.common.application.IOBaseApp
import co.topl.grpc.ToplGrpc
import co.topl.models.{BlockHeaderV2, TypedIdentifier}
import co.topl.testnetsimulationorchestrator.algebras.DataPublisher
import co.topl.testnetsimulationorchestrator.interpreters.{GcpCsvDataPublisher, K8sSimulationController}
import co.topl.testnetsimulationorchestrator.models.{AdoptionDatum, BlockDatum, TransactionDatum}
import co.topl.typeclasses.implicits._
import fs2._
import fs2.concurrent.Topic
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration._

object Orchestrator
    extends IOBaseApp[Args, ApplicationConfig](
      createArgs = args => Args.parserArgs.constructOrThrow(args),
      createConfig = IOBaseApp.createTypesafeConfig,
      parseConfig = (args, conf) => ApplicationConfig.unsafe(args, conf)
    ) {

  private type NodeRpcs = Map[String, ToplRpc[F, Stream[F, *]]]

  private type Publisher = DataPublisher[F, Stream[F, *]]

  implicit private val logger: Logger[F] =
    Slf4jLogger.getLoggerFromClass[F](this.getClass)

  def run: IO[Unit] =
    for {
      _ <- Logger[F].info("Launching Testnet Simulation Orchestrator")
      _ <- Logger[F].info(show"args=$args")
      _ <- Logger[F].info(show"config=$appConfig")
      _ <- runSimulation
    } yield ()

  /**
   * Resources required to run this simulation and cleanup
   */
  private def resources: Resource[F, (Publisher, NodeRpcs)] =
    // Allocate the K8sSimulationController resource first
    K8sSimulationController
      .resource[F](appConfig.simulationOrchestrator.kubernetes.namespace)
      // In addition, run the "terminate" operation to tear down the k8s namespace regardless of the outcome of the simulation
      .flatMap(c => Resource.onFinalize(c.terminate))
      .productR(
        (
          GcpCsvDataPublisher.make[F](
            appConfig.simulationOrchestrator.publish.bucket,
            s"${appConfig.simulationOrchestrator.publish.filePrefix}${System.currentTimeMillis()}/"
          ),
          appConfig.simulationOrchestrator.nodes
            .parTraverse(n =>
              ToplGrpc.Client
                .make[F](n.host, n.port, tls = false)
                .evalTap(awaitNodeReady(n.name, _))
                .tupleLeft(n.name)
            )
            .map(_.toMap)
        ).tupled
      )

  private def awaitNodeReady(name: String, client: ToplRpc[F, Stream[F, *]]) =
    Logger[F].info(show"Awaiting readiness of node=$name") >>
    Stream.retry(client.blockIdAtHeight(1), 250.milli, identity, 200).compile.drain >>
    Logger[F].info(show"Node node=$name is ready")

  private def runSimulation: F[Unit] =
    resources.use { case (publisher, nodes) =>
      for {
        // Listen to the block adoptions of all nodes.
        nodeHeaderAdoptions <- fetchHeaderAdoptions(nodes)

        // Once we have all of the adoptions, publish them
        _ <- nodeHeaderAdoptions.parTraverse { case (node, adoptions) =>
          Logger[F].info(show"Publishing adoptions for node=$node") >>
          publisher.publishAdoptions(Stream.iterable(adoptions).map(a => AdoptionDatum(a._1, a._2)), node)
        }.void

        // Each node produced a stream of adoptions.  Ideally, the streams contain duplicates, assuming the nodes
        // are in-sync.  To avoid writing duplicate blocks to the published results, deduplicate them, and assign to
        // the node that adopted first.
        blockAssignments <- assignBlocksToNodes(nodeHeaderAdoptions)

        // Similarly, deduplicate transaction-fetching assignments
        transactionAssignments <- publishBlockBodiesAndAssignTransactions(publisher, nodes)(blockAssignments)

        // Assemble the assignments into a stream, fetch the transaction, and convert into a TransactionDatum
        transactionDatumStream = Stream
          .iterable(transactionAssignments.toList)
          .parEvalMap[F, TransactionDatum](Runtime.getRuntime.availableProcessors()) { case (transactionId, node) =>
            OptionT(nodes(node).fetchTransaction(transactionId))
              .getOrRaise(new NoSuchElementException(show"Transaction not found id=$transactionId"))
              .map(TransactionDatum(_))
          }

        _ <- Logger[F].info("Fetching and publishing transactions")
        // Now publish the transaction results
        _ <- publisher.publishTransactions(transactionDatumStream)

        // Simulation complete :)
      } yield ()
    }

  /**
   * Listen to the streams of block ID adoptions from all nodes in parallel.  Simultaneously, fetch each corresponding
   * header to determine its height, which is then used to determine when to stop.
   */
  private def fetchHeaderAdoptions(nodes: NodeRpcs): F[List[(String, Vector[(TypedIdentifier, Long, BlockHeaderV2)])]] =
    nodes.toList.parTraverse { case (name, client) =>
      for {
        _          <- Logger[F].info(show"Fetching adoptions+headers from node=$name")
        baseStream <- client.synchronizationTraversal()
        stream = baseStream.collect { case SynchronizationTraversalSteps.Applied(id) => id }
        headers <- stream
          .zip(Stream.repeatEval(Async[F].defer(Async[F].realTimeInstant)))
          .evalMap { case (id, adoptionTimestamp) =>
            OptionT(client.fetchBlockHeader(id))
              .getOrRaise(new NoSuchElementException(id.show))
              .map((id, adoptionTimestamp.toEpochMilli, _))
          }
          // Stop listening once a node adopts a block at the target height.
          .takeWhile(_._3.height <= appConfig.simulationOrchestrator.scenario.targetHeight)
          .compile
          .toVector
        _ <- Logger[F].info(show"Finished fetching adoptions+headers from node=$name")
      } yield (name -> headers)
    }

  private def assignBlocksToNodes(
    nodeBlockAdoptions: List[(String, Vector[(TypedIdentifier, Long, BlockHeaderV2)])]
  ): F[List[(String, TypedIdentifier, BlockHeaderV2)]] =
    Sync[F].delay(
      nodeBlockAdoptions
        .flatMap { case (node, adoptions) => adoptions.map { case (id, _, header) => (node, id, header) } }
        .groupBy(_._2)
        .values
        .map(_.head)
        .toList
        .sortBy(_._3.height)
    )

  private def publishBlockBodiesAndAssignTransactions(publisher: Publisher, nodes: NodeRpcs)(
    blockAssignments:                                            List[(String, TypedIdentifier, BlockHeaderV2)]
  ) =
    for {
      // Create a topic which is expected to contain two subscribers
      blockDatumTopic <- Topic[F, (String, BlockDatum)]
      // Assemble a stream of BlockDatum by using the list of blockAssignments, fetching the body, and converting into
      // a BlockDatum
      blockDatumSourceStream = Stream
        .iterable(blockAssignments)
        .parEvalMap[F, (String, BlockDatum)](Runtime.getRuntime.availableProcessors()) { case (node, id, header) =>
          OptionT(nodes(node).fetchBlockBody(id))
            .getOrRaise(new NoSuchElementException(show"Block Body not found id=$id"))
            .map(body => node -> BlockDatum(header, body))
        }
        .through(blockDatumTopic.publish)
      assignTransactionsStream =
        blockDatumTopic
          .subscribe(128)
          .fold(Map.empty[TypedIdentifier, String]) { case (assignments, (node, datum)) =>
            assignments ++ datum.bodyV2.toList.tupleRight(node)
          }
      // Publish the block data results
      _ <- Logger[F].info("Fetching block bodies, publishing blocks, and assigning transactions (in parallel)")
      (_, _, transactionAssignments) <- (
        blockDatumSourceStream.compile.drain,
        publisher.publishBlocks(blockDatumTopic.subscribe(128).map(_._2)),
        assignTransactionsStream.compile.lastOrError
      ).parTupled
    } yield transactionAssignments
}
