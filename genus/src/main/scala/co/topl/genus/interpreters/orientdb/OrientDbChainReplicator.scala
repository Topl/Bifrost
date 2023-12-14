package co.topl.genus.interpreters.orientdb

import akka.stream.scaladsl.{Keep, RunnableGraph, Sink, Source}
import cats.data.OptionT
import cats.effect.{Async, Sync}
import cats.implicits._
import cats.{~>, Applicative, Parallel, Traverse}
import co.topl.algebras.ToplRpc
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.genus.algebras.ChainReplicatorAlgebra
import co.topl.genus.interpreters.orientdb.EdgeSchemas._
import co.topl.genus.interpreters.orientdb.NodeSchemas._
import co.topl.genus.interpreters.orientdb.OrientDb._
import co.topl.models.{BlockHeaderV2, Transaction, TypedIdentifier}
import co.topl.typeclasses.implicits._
import com.tinkerpop.blueprints.Direction
import com.tinkerpop.blueprints.impls.orient.OrientBaseGraph
import org.typelevel.log4cats.Logger

object OrientDbChainReplicator {

  def make[F[_]: Async: Logger: Parallel: FToFuture: RunnableGraph ~> *[_]](
    graph: OrientBaseGraph
  ): F[ChainReplicatorAlgebra[F, SourceMatNotUsed]] = {
    Async[F].defer {
      graph.initializeSchema(NodeSchemas.canonicalHeadNodeSchema) >>
      graph.initializeSchema(NodeSchemas.headerNodeSchema) >>
      graph.initializeSchema(NodeSchemas.transactionNodeSchema) >>
      graph.initializeSchema(NodeSchemas.transactionInputNodeSchema) >>
      graph.initializeSchema(NodeSchemas.transactionOutputNodeSchema) >>
      graph.initializeSchema(EdgeSchemas.canonicalHeadEdgeSchema) >>
      graph.initializeSchema(EdgeSchemas.headerToParentEdgeSchema) >>
      graph.initializeSchema(EdgeSchemas.transactionToHeaderEdgeSchema) >>
      graph.initializeSchema(EdgeSchemas.transactionToInputEdgeSchema) >>
      graph.initializeSchema(EdgeSchemas.transactionToOutputEdgeSchema)
    } >>
    Async[F].delay {
      new ChainReplicatorAlgebra[F, SourceMatNotUsed] {
        private type RpcClient = ToplRpc[F, SourceMatNotUsed]
        def replicateFrom(client: ToplRpc[F, SourceMatNotUsed]): F[Unit] =
          Logger[F].info("Replicating history") >>
          replicateHistory(client) >>
          Logger[F].info("Replicating live data") >>
          replicateLive(client) >>
          Logger[F].info("Done replicating")

        /**
         * Replicates past blockchain data (up to the node's _current_ head) into the graph database
         */
        private def replicateHistory(client: RpcClient): F[Unit] =
          for {
            adoptionsSource <- client.blockAdoptions()
            latestBlockId   <- Async[F].fromFuture(adoptionsSource.toMat(Sink.head)(Keep.right).liftTo[F])
            latestHeader    <- OptionT(client.fetchHeader(latestBlockId)).getOrElse(???)
            graphHeight     <- currentGraphHeight()
            missingHeights = Range.Long.inclusive(graphHeight.fold(1L)(_ + 1), latestHeader.height, 1)
            _ <- Async[F].fromFuture(
              Source(missingHeights)
                .tapAsyncF(1)(height =>
                  for {
                    id             <- OptionT(client.fetchBlockIdAtHeight(height)).getOrElse(???)
                    (header, body) <- fetchSingle(client)(id)
                    _              <- save(id, header, body)
                    _ <- (height === missingHeights.last)
                      .pure[F]
                      .ifM(
                        setCanonicalHead(stringifyId(id)),
                        Applicative[F].unit
                      )
                  } yield ()
                )
                .toMat(Sink.ignore)(Keep.right)
                .liftTo[F]
            )
          } yield ()

        /**
         * Replicates blockchain data (and missing ancestors) as it is announced by the node
         */
        private def replicateLive(client: RpcClient): F[Unit] =
          Async[F]
            .fromFuture(
              client
                .blockAdoptions()
                .flatMap(
                  _.mapAsyncF(1)(fetchRecursively(client))
                    .tapAsyncF(1)(items =>
                      items.traverse((save _).tupled) >>
                      setCanonicalHead(stringifyId(items.last._1))
                    )
                    .toMat(Sink.ignore)(Keep.right)
                    .liftTo[F]
                )
            )
            .void

        private def fetchSingle(client: RpcClient)(
          id:                           TypedIdentifier
        ): F[(BlockHeaderV2, List[Transaction])] =
          OptionT(client.fetchHeader(id))
            .toRight(new NoSuchElementException(s"Header id=${id.show}"))
            .rethrowT
            .parProduct(
              OptionT(client.fetchBody(id))
                .toRight(new NoSuchElementException(s"Body id=${id.show}"))
                .rethrowT
                .flatMap(body =>
                  body.toList.parTraverse(transactionId =>
                    OptionT(client.fetchTransaction(transactionId))
                      .toRight(new NoSuchElementException(s"Transaction id=${transactionId.show}"))
                      .rethrowT
                  )
                )
            )

        /**
         * Recursively fetch the block (and its transactions) and its ancestors until an ancestor is found in the local
         * database.
         */
        private def fetchRecursively(client: RpcClient)(
          id:                                TypedIdentifier
        ): F[List[(TypedIdentifier, BlockHeaderV2, Seq[Transaction])]] =
          for {
            idExists <- OptionT(getGraphHeader(stringifyId(id))).isDefined
            (_, result, _) <- (id, List.empty[(TypedIdentifier, BlockHeaderV2, Seq[Transaction])], idExists)
              .iterateUntilM[F] { case (id, accumulator, _) =>
                for {
                  (header, transactions) <- fetchSingle(client)(id)
                  parentExists           <- OptionT(getGraphHeader(stringifyId(header.parentHeaderId))).isDefined
                } yield (
                  header.parentHeaderId,
                  (id, header, transactions) +: accumulator,
                  parentExists || header.height <= 1
                )
              }(_._3)
          } yield result

        private def save(
          id:           TypedIdentifier,
          header:       BlockHeaderV2,
          transactions: Seq[Transaction]
        ): F[Unit] =
          for {
            headerIdString <- stringifyId(id).pure[F]
            _              <- saveHeader(headerIdString, header)
            _              <- saveTransactions(headerIdString, transactions)
          } yield ()

        private def saveHeader(idString: String, header: BlockHeaderV2) =
          for {
            _ <- Logger[F].info(show"Saving header id=$idString")
            node = NodeTypes.Header(
              idString,
              header.txRoot.data.toBase58,
              header.timestamp,
              header.height,
              header.slot,
              header.address.immutableBytes.toBase58
            )
            _ <- (header.height <= 1L)
              .pure[F]
              .ifM(
                graph.insertNode(node),
                Logger[F].info(
                  show"Associating header id=$idString to parent=${stringifyId(header.parentHeaderId)}"
                ) >>
                graph
                  .insertNodeBuilder(node)
                  .withEdgeTo(
                    EdgeTypes.HeaderToParentHeader,
                    NodesByClass[NodeTypes.Header](
                      Where.PropEquals("blockId", stringifyId(header.parentHeaderId))
                    )
                  )
                  .run()
              )
          } yield ()

        private def saveTransactions[G[_]: Traverse](blockIdString: String, transactions: G[Transaction]) =
          transactions.traverseWithIndexM((transaction, index) =>
            for {
              transactionIdString <- stringifyId(transaction.id.asTypedBytes).pure[F]
              _                   <- Logger[F].info(show"Saving transaction id=$transactionIdString")
              _ <- graph.insertNode(
                NodeTypes.Transaction(
                  transactionIdString,
                  transaction.chronology.creation,
                  transaction.chronology.minimumSlot,
                  transaction.chronology.maximumSlot,
                  transaction.data.map(_.data.value)
                )
              )
              _ <- Logger[F].info(show"Connecting transaction id=$transactionIdString to header id=$blockIdString")
              _ <- graph.insertEdge(
                EdgeTypes.TransactionToHeader(index.toShort),
                NodesByClass[NodeTypes.Transaction](Where.PropEquals("transactionId", transactionIdString)),
                NodesByClass[NodeTypes.Header](Where.PropEquals("blockId", blockIdString))
              )
              _ <- transaction.inputs.traverseWithIndexM((input, inputIndex) =>
                Logger[F].info(
                  show"Creating transaction input id=$transactionIdString index=$inputIndex"
                ) >>
                Logger[F].info(
                  show"Connecting input of transaction inputTransactionId=${transactionIdString} inputIndex=$inputIndex" +
                  show" to previous outputTransactionId=${input.boxId.transactionId} outputTransactionIndex=${input.boxId.transactionOutputIndex}"
                ) >>
                Sync[F].blocking {
                  val inputV = graph.addVertex(s"class:${NodeSchemas.transactionInputNodeSchema.name}")
                  NodeSchemas.transactionInputNodeSchema
                    .encode(
                      NodeTypes.TransactionInput(
                        input.proposition.immutableBytes.toBase58,
                        input.proof.immutableBytes.toBase58
                      )
                    )
                    .foreachEntry(inputV.setProperty)
                  inputV.save()
                  val transactionInputV = graph
                    .getVertices("Transaction", Array("transactionId"), Array(transactionIdString))
                    .iterator()
                    .next()
                  val e = transactionInputV.addEdge(s"class:${EdgeSchemas.transactionToInputEdgeSchema}", inputV)
                  EdgeSchemas.transactionToInputEdgeSchema
                    .encode(
                      EdgeTypes.TransactionToInput(index.toShort)
                    )
                    .foreachEntry(e.setProperty)
                  import scala.jdk.CollectionConverters._
                  val spentTransactionInput = graph
                    .getVertices("Transaction", Array("transactionId"), Array(stringifyId(input.boxId.transactionId)))
                    .iterator()
                    .next()
                    .getEdges(Direction.OUT)
                    .iterator()
                    .asScala
                    .find(e => e.getProperty[Short]("index") === input.boxId.transactionOutputIndex)
                    .get
                    .getVertex(Direction.IN)
                  val e2 = inputV.addEdge(s"class:${EdgeSchemas.inputToOutputEdgeSchema}", spentTransactionInput)
                  EdgeSchemas.inputToOutputEdgeSchema.encode(EdgeTypes.InputToOutput).foreachEntry(e2.setProperty)
                }
              )
              _ <- transaction.outputs.traverseWithIndexM((output, outputIndex) =>
                Logger[F].info(show"Creating transaction output id=$transactionIdString index=$outputIndex") >>
                graph
                  .insertNodeBuilder(
                    NodeTypes.TransactionOutput(
                      output.address.immutableBytes.toBase58,
                      output.minting
                    )
                  )
                  .withEdgeFrom(
                    EdgeTypes.TransactionToOutput(outputIndex.toShort),
                    NodesByClass[NodeTypes.Transaction](Where.PropEquals("transactionId", transactionIdString))
                  )
                  .run()
              )
            } yield ()
          )

        private def setCanonicalHead(idString: String): F[Unit] =
          Logger[F].info(show"Setting canonical head id=$idString") >>
          graph.removeNodes(NodesByClass[NodeTypes.CanonicalHead.type]()) >>
          graph
            .insertNodeBuilder(NodeTypes.CanonicalHead)
            .withEdgeTo(EdgeTypes.CanonicalHead, NodesByClass[NodeTypes.Header](Where.PropEquals("blockId", idString)))
            .run()

        private def currentGraphHeight(): F[Option[Long]] =
          OptionT(
            graph.getNode(
              Trace[NodeTypes.CanonicalHead.type]()
                .out[EdgeTypes.CanonicalHead.type, NodeTypes.Header]
            )
          ).map(_.height).value

        private def getGraphHeader(id: String): F[Option[NodeTypes.Header]] =
          graph.getNode(
            NodesByClass[NodeTypes.Header](Where.PropEquals("blockId", id))
          )

        private def getGraphTransaction(id: String): F[Option[NodeTypes.Transaction]] =
          graph.getNode(
            NodesByClass[NodeTypes.Transaction](Where.PropEquals("transactionId", id))
          )

        private def stringifyId(id: TypedIdentifier) =
          id.allBytes.toBase58
      }
    }
  }
}
