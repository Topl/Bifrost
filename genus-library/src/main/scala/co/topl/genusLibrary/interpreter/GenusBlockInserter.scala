package co.topl.genusLibrary.interpreter

import cats.data.{Chain, EitherT}
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.codecs.bytes.tetra.TetraIdentifiableInstances
import co.topl.genusLibrary.{Txo, TxoState}
import co.topl.genusLibrary.algebras.{BlockInserterAlgebra, StoreResponse}
import co.topl.genusLibrary.failure.{Failure, Failures}
import co.topl.genusLibrary.orientDb.GenusGraphMetadata._
import co.topl.genusLibrary.orientDb.wrapper.OpenForCommitsBatchGraph
import co.topl.genusLibrary.orientDb.{OrientDBFacade, VertexSchema}
import co.topl.models.BlockBodyV2.Full
import co.topl.models.Transaction.{Input, Output}
import co.topl.models.{BlockBodyV2, BlockHeaderV2, BlockV2, Box, Transaction, TypedBytes, TypedIdentifier}
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.OrientGraph
import com.tinkerpop.blueprints.util.wrappers.batch.BatchGraph
import com.typesafe.scalalogging.LazyLogging
import scodec.bits.ByteVector
import co.topl.typeclasses.implicits._

import scala.collection.immutable.ListSet
import scala.jdk.javaapi.CollectionConverters.asScala

class GenusBlockInserter[F[_]: Async](
  orientDB: OrientDBFacade
) extends BlockInserterAlgebra[F]
    with LazyLogging {

  override def insert(block: BlockV2.Full): StoreResponse[F, Unit] = {

    val body = block.transactions
    val header = block.headerV2

    val batchGraphAndUpdatableInputs: F[Either[
      Failure,
      (
        OpenForCommitsBatchGraph,
        Chain[() => (Txo, Vertex)]
      )
    ]] = (for {

      // There should be only one block header with the given id
      previousHeaderVertex <- EitherT(fetchPreviousHeaderVertex(header))

      inputVertices <- EitherT(fetchInputVertices(body))

      batchGraph = new OpenForCommitsBatchGraph(orientDB.getBatchGraph)

      (_, currentHeaderVertex) = batchGraph.createVertex(header)

      _ = batchGraph.addEdge(previousHeaderVertex, currentHeaderVertex)

      (_, currentBodyVertex) = batchGraph.createVertex(ListSet.empty ++ body.toList.map(toId))

      _ = batchGraph.addEdge(currentHeaderVertex, currentBodyVertex)

      transactionVertices = body.map(transaction => batchGraph.createVertex(transaction))

      _ = transactionVertices.toList.foreach { case (_, transactionVertex) =>
        batchGraph.addEdge(currentHeaderVertex, transactionVertex)
      }

      updatableInputs = transactionVertices.flatMap { case (transaction, transactionVertex) =>
        val txId = toId(transaction)

        transaction.outputs.zipWithIndex
          .map { case (output, idx) =>
            toTxo(output, Box.Id(toId(transaction), idx.toShort))
          }
          .map(txo => batchGraph.createVertex(txo))
          .toList
          .foreach { case (_, outputVertex) =>
            batchGraph.addEdge(transactionVertex, outputVertex, "Output".some)
          }

        inputVertices
          .filter(_.getProperty("transactionId") == txId.dataBytes.toArray)
          .map { inputVertex =>
            batchGraph.addEdge(transactionVertex, inputVertex, "Input".some)
            val propertiesByKey =
              asScala(inputVertex.getPropertyKeys).map(key => key -> inputVertex.getProperty(key)).toMap
            val txoInput = txoSchema.decode(propertiesByKey)
            val updatedTxoInput = txoInput.copy(state = TxoState.Spent)
            () => batchGraph.updateVertex(updatedTxoInput, inputVertex)
          }
      }

    } yield (batchGraph, updatableInputs)).value

    batchGraphAndUpdatableInputs map {
      case Left(e) =>
        logger error s"Unexpected error while trying to get batch graph. Error=[$e]"
        e.asLeft[Unit]
      case Right((batchGraph, updatableInputs)) =>
        batchGraph.commit()
        updatableInputs.toList.foreach(_.apply())
          .asRight[Failure]
    }
  }

  private def fetchInputVertices(body: Full): F[Either[Failure, Chain[Vertex]]] =
    body
      .flatMap(transaction =>
        transaction.inputs.map { case Input(boxId, _, _, _) =>
          Set.apply[(String, AnyRef)](
            ("transactionId", toId(transaction).dataBytes.toArray),
            ("transactionOutputIndex", boxId.transactionOutputIndex.asInstanceOf[AnyRef])
          )
        }
      )
      .traverse(e => orientDB.getVertex(txoSchema.name, e).map((e, _)))
      .map(_.foldLeft(Chain.empty[Vertex].asRight[Set[Set[(String, AnyRef)]]]) {
        case (Right(vertices), (_, Some(inputVertex))) => (vertices :+ inputVertex) asRight
        case (Right(_), (queryParameters, None))       => Set(queryParameters) asLeft
        case (left @ Left(_), (_, Some(_)))            => left
        case (Left(failedQueryParameters), (queryParameters, None)) =>
          (failedQueryParameters + queryParameters) asLeft
      })
      .map {
        case Left(failedQueryParameters) => Failures.MissingInputVerticesFailure(failedQueryParameters) asLeft
        case Right(inputVertices)        => inputVertices asRight
      }

  private def fetchPreviousHeaderVertex(header: BlockHeaderV2) =
    orientDB
      .getVertex(blockHeaderSchema.name, "blockId", header.parentHeaderId.asInstanceOf[AnyRef])
      .map(
        _.toRight(
          Failures.NoPreviousHeaderVertexFailure(
            TetraIdentifiableInstances.identifiableBlockHeaderV2.idOf(header),
            header.parentHeaderId
          )
        )
      )

  private def toTxo(output: Output, boxId: Box.Id): Txo = Txo(
    box = Box(output.address.spendingAddress.typedEvidence, output.value),
    state = TxoState.Unspent,
    id = boxId,
    address = output.address.spendingAddress.some
  )

  private def toId(transaction: Transaction): TypedIdentifier = {
    val (typePrefix, bytes) = TetraIdentifiableInstances.transactionIdentifiable.idOf(transaction)
    TypedBytes(ByteVector(typedBytesTupleToByteArray((typePrefix, bytes.toArray))))
  }

}
