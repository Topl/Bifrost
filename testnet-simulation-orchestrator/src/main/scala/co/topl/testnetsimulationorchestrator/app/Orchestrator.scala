package co.topl.testnetsimulationorchestrator.app

import cats.data.OptionT
import cats.effect._
import cats.implicits._
import co.topl.algebras.{SynchronizationTraversalSteps, ToplRpc}
import co.topl.common.application.IOBaseApp
import co.topl.grpc.ToplGrpc
import co.topl.models.TypedIdentifier
import co.topl.testnetsimulationorchestrator.algebras.DataPublisher
import co.topl.testnetsimulationorchestrator.interpreters.{GcpCsvDataPublisher, K8sSimulationController}
import co.topl.testnetsimulationorchestrator.models.{AdoptionDatum, BlockDatum, TransactionDatum}
import co.topl.typeclasses.implicits._
import fs2._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration._

object Orchestrator
    extends IOBaseApp[Args, ApplicationConfig](
      createArgs = args => Args.parserArgs.constructOrThrow(args),
      createConfig = IOBaseApp.createTypesafeConfig,
      parseConfig = (args, conf) => ApplicationConfig.unsafe(args, conf)
    ) {

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
  private def resources: Resource[F, (DataPublisher[F, Stream[F, *]], Map[String, ToplRpc[F, Stream[F, *]]])] =
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
        nodeBlockAdoptions <- nodes.toList.parTraverse { case (name, client) =>
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

        // Once we have all of the adoptions, publish them
        _ <- nodeBlockAdoptions.parTraverse { case (node, adoptions) =>
          Logger[F].info(show"Publishing adoptions for node=$node") >>
          publisher.publishAdoptions(Stream.iterable(adoptions).map(a => AdoptionDatum(a._1, a._2)), node)
        }

        // Each node produced a stream of adoptions.  Ideally, the streams contain duplicates, assuming the nodes
        // are in-sync.  To avoid writing duplicate blocks to the published results, deduplicate them, and assign to
        // the node that adopted first.
        blockAssignments = nodeBlockAdoptions
          .flatMap { case (node, adoptions) => adoptions.map { case (id, _, header) => (node, id, header) } }
          .groupBy(_._2)
          .values
          .map(_.head)
          .toList
          .sortBy(_._3.height)

        // Similarly, multiple nodes adopt the same block (body) which would result in duplicate transaction IDs
        // in a flattened stream.  Furthermore, two different block bodies may overlap on some of the Transaction IDs.
        // Those need to be further deduplicated.  Instead of doing that as a synchronous/pure operation, build
        // the collection lazily in the block "publish" stream
        transactionAssignmentsRef <- IO.ref(Map.empty[TypedIdentifier, String])

        _ <- Logger[F].info("Fetching block bodies")
        // Assemble a stream of BlockDatum by using the list of blockAssignments, fetching the body, and converting into
        // a BlockDatum
        blockDatumStream = Stream
          .iterable(blockAssignments)
          .evalMap { case (node, id, header) =>
            OptionT(nodes(node).fetchBlockBody(id))
              .getOrRaise(new NoSuchElementException(show"Block Body not found id=$id"))
              .map(body => node -> BlockDatum(header, body))
          }
          .evalTap { case (node, datum) =>
            // As a side-effect, update the transaction assignments
            transactionAssignmentsRef.update(_ ++ datum.bodyV2.toList.tupleRight(node))
          }
          .map(_._2)

        // Publish the block data results
        _ <- Logger[F].info("Publishing blocks")
        _ <- publisher.publishBlocks(blockDatumStream)

        // Now that the block data has been published, the side-effecting operations to assign transactions
        // is also complete
        transactionAssignments <- transactionAssignmentsRef.get

        // Assemble the assignments into a stream, fetch the transaction, and convert into a TransactionDatum
        transactionDatumStream = Stream.iterable(transactionAssignments.toList).evalMap { case (transactionId, node) =>
          OptionT(nodes(node).fetchTransaction(transactionId))
            .getOrRaise(new NoSuchElementException(show"Transaction not found id=$transactionId"))
            .map(TransactionDatum(_))
        }

        _ <- Logger[F].info("Fetching and publishing block bodies")
        // Now publish the transaction results
        _ <- publisher.publishTransactions(transactionDatumStream)

        // Simulation complete :)
      } yield ()
    }

}
