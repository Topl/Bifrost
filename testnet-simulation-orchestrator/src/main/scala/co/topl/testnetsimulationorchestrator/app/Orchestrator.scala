package co.topl.testnetsimulationorchestrator.app

import cats.data.OptionT
import cats.effect._
import cats.implicits._
import co.topl.common.application.IOBaseApp
import co.topl.grpc.ToplGrpc
import co.topl.models.TypedIdentifier
import co.topl.testnetsimulationorchestrator.interpreters.{GcpCsvDataPublisher, K8sSimulationController}
import co.topl.testnetsimulationorchestrator.models.{AdoptionDatum, BlockDatum, TransactionDatum}
import co.topl.typeclasses.implicits._
import fs2._

object Orchestrator
    extends IOBaseApp[Args, ApplicationConfig](
      createArgs = args => Args.parserArgs.constructOrThrow(args),
      createConfig = IOBaseApp.createTypesafeConfig,
      parseConfig = (args, conf) => ApplicationConfig.unsafe(args, conf)
    ) {

  def run: IO[Unit] =
    // Allocate the K8sSimulationController resource first
    K8sSimulationController
      .resource[F](appConfig.simulationOrchestrator.kubernetes.namespace)
      // In addition, run the "terminate" operation to tear down the k8s namespace regardless of the outcome of the simulation
      .flatMap(c => Resource.onFinalize(c.terminate))
      .flatMap(_ =>
        (
          GcpCsvDataPublisher.make[F](
            appConfig.simulationOrchestrator.publish.bucket,
            appConfig.simulationOrchestrator.publish.filePrefix
          ),
          appConfig.simulationOrchestrator.nodes
            .traverse(n => ToplGrpc.Client.make[F](n.host, n.port, tls = false).map(n.name -> _))
            .map(_.toMap)
        ).tupled
      )
      .use { case (publisher, nodes) =>
        for {
          // Listen to the block adoptions of all nodes.
          nodeBlockAdoptions <- nodes.toList.parTraverse { case (name, client) =>
            for {
              stream <- (??? : Stream[F, TypedIdentifier]).pure[F]
              headers <- stream
                .zip(Stream.repeatEval(Async[F].realTimeInstant))
                .evalMap { case (id, adoptionTimestamp) =>
                  OptionT(client.fetchBlockHeader(id))
                    .getOrRaise(new NoSuchElementException(id.show))
                    .map((id, adoptionTimestamp.toEpochMilli, _))
                }
                // Stop listening once a node adopts a block at the target height.
                .takeWhile(_._3.height <= appConfig.simulationOrchestrator.scenario.targetHeight)
                .compile
                .toVector
            } yield (name -> headers)
          }

          // Once we have all of the adoptions, publish them
          _ <- nodeBlockAdoptions.parTraverse { case (node, adoptions) =>
            publisher.publishAdoptions(Stream.iterable(adoptions).map(a => AdoptionDatum(a._1, a._2)), node)
          }

          // Each node produced a stream of adoptions.  Ideally, the streams contain duplicates, assuming the nodes
          // are in-sync.  To avoid writing duplicate blocks to the published results, deduplicate them, and assign to
          // the node that adopted first.
          blockAssignments = nodeBlockAdoptions
            .flatMap { case (node, adoptions) => adoptions.map { case (id, _, header) => (node, id, header) } }
            .groupBy(_._1)
            .values
            .map(_.head)
            .toList

          // Similarly, multiple nodes adopt the same block (body) which would result in duplicate transaction IDs
          // in a flattened stream.  Furthermore, two different block bodies may overlap on some of the Transaction IDs.
          // Those need to be further deduplicated.  Instead of doing that as a synchronous/pure operation, build
          // the collection lazily in the block "publish" stream
          transactionAssignmentsRef <- IO.ref(Map.empty[TypedIdentifier, String])

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
          _ <- publisher.publishBlocks(blockDatumStream)

          // Now that the block data has been published, the side-effecting operations to assign transactions
          // is also complete
          transactionAssignments <- transactionAssignmentsRef.get

          // Assemble the assignments into a stream, fetch the transaction, and convert into a TransactionDatum
          transactionDatumStream = Stream.iterable(transactionAssignments.toList).evalMap {
            case (transactionId, node) =>
              OptionT(nodes(node).fetchTransaction(transactionId))
                .getOrRaise(new NoSuchElementException(show"Transaction not found id=$transactionId"))
                .map(TransactionDatum(_))
          }

          // Now publish the transaction results
          _ <- publisher.publishTransactions(transactionDatumStream)

          // Simulation complete :)
        } yield ()
      }
}
