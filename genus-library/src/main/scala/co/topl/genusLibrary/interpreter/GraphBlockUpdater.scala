package co.topl.genusLibrary.interpreter

import cats.Monad
import cats.data.EitherT
import cats.effect._
import cats.implicits._
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax.{
  groupPolicyAsGroupPolicySyntaxOps,
  ioTransactionAsTransactionSyntaxOps,
  seriesPolicyAsSeriesPolicySyntaxOps
}
import co.topl.genus.services.{BlockData, Txo, TxoState}
import co.topl.genusLibrary.algebras.{BlockFetcherAlgebra, BlockUpdaterAlgebra, NodeBlockFetcherAlgebra}
import co.topl.genusLibrary.model.{GE, GEs}
import co.topl.genusLibrary.orientDb.OrientThread
import co.topl.genusLibrary.orientDb.instances.SchemaBlockHeader.Field
import co.topl.genusLibrary.orientDb.instances.VertexSchemaInstances.instances._
import co.topl.genusLibrary.orientDb.instances.{SchemaIoTransaction, SchemaLockAddress, SchemaTxo}
import co.topl.genusLibrary.orientDb.schema.EdgeSchemaInstances._
import co.topl.models.utility._
import co.topl.node.models.BlockBody
import co.topl.typeclasses.implicits._
import com.tinkerpop.blueprints.impls.orient.OrientGraph
import fs2.Stream
import org.typelevel.log4cats.Logger

import scala.util.Try

object GraphBlockUpdater {

  def make[F[_]: OrientThread: Monad: Logger](
    graph:            OrientGraph,
    blockFetcher:     BlockFetcherAlgebra[F],
    nodeBlockFetcher: NodeBlockFetcherAlgebra[F, Stream[F, *]]
  ): Resource[F, BlockUpdaterAlgebra[F]] =
    Resource.pure(
      new BlockUpdaterAlgebra[F] {

        def insert(block: BlockData): F[Either[GE, Unit]] =
          if (block.header.height == 1) {
            insertBlock(block)
          } else {
            EitherT(blockFetcher.fetchBlock(block.header.parentHeaderId)).flatMapF {
              case Some(_) => insertBlock(block)
              case _ =>
                Logger[F].info(s"Block parent ${block.header.parentHeaderId.show} not found on Genus, inserting it") >>
                EitherT(nodeBlockFetcher.fetch(block.header.parentHeaderId))
                  .flatMapF(insert)
                  .flatMapF(_ => insertBlock(block))
                  .value
            }.value

          }

        private def insertBlock(block: BlockData): F[Either[GE, Unit]] =
          OrientThread[F].delay {
            Try {
              val headerVertex = graph.addBlockHeader(block.header)

              graph.addCanonicalHead(headerVertex)

              // Relationships between Header <-> Body
              val body = BlockBody(block.body.transactions.map(_.id), block.body.rewardTransaction.map(_.id))
              val bodyVertex = graph.addBody(body)
              bodyVertex.setProperty(blockBodySchema.links.head.propertyName, headerVertex.getId)
              graph.addEdge(s"class:${blockHeaderBodyEdge.name}", headerVertex, bodyVertex, blockHeaderBodyEdge.label)

              def insertTx(ioTx: IoTransaction, isReward: Boolean): Unit = {
                val ioTxVertex = graph.addIoTx(ioTx)
                ioTxVertex.setProperty(ioTransactionSchema.links.head.propertyName, headerVertex.getId)
                ioTxVertex.setProperty(SchemaIoTransaction.Field.IsReward, isReward)
                if (isReward) {
                  graph.addEdge(
                    s"class:${blockHeaderRewardEdge.name}",
                    headerVertex,
                    ioTxVertex,
                    blockHeaderRewardEdge.label
                  )
                } else {
                  graph.addEdge(
                    s"class:${blockHeaderTxIOEdge.name}",
                    headerVertex,
                    ioTxVertex,
                    blockHeaderTxIOEdge.label
                  )

                  // Lookup previous unspent TXOs, and update the state
                  ioTx.inputs.zipWithIndex.foreach { case (spentTransactionOutput, inputIndex) =>
                    graph
                      .getTxo(spentTransactionOutput.address)
                      .foreach { vertex =>
                        vertex.setProperty(SchemaTxo.Field.State, TxoState.SPENT.value)
                        vertex.setProperty(SchemaTxo.Field.SpendingTransaction, ioTxVertex.getId)
                        vertex.setProperty(SchemaTxo.Field.SpendingInputIndex, java.lang.Integer.valueOf(inputIndex))
                      }
                  }
                }

                // Relationships between TxIOs <-> LockAddress
                ioTx.outputs.zipWithIndex.foreach { case (utxo, index) =>
                  // before adding a new address, check if was not there included by a previous transaction
                  val lockAddressVertex = {
                    val addressIterator =
                      graph.getVertices(SchemaLockAddress.Field.AddressId, utxo.address.id.value.toByteArray).iterator()

                    if (addressIterator.hasNext) addressIterator.next()
                    else graph.addLockAddress(utxo.address)

                  }
                  graph.addEdge(s"class:${addressTxIOEdge.name}", lockAddressVertex, ioTxVertex, addressTxIOEdge.label)

                  val txoVertex = graph.addTxo(
                    Txo(
                      utxo,
                      TxoState.UNSPENT,
                      TransactionOutputAddress(utxo.address.network, utxo.address.ledger, index, ioTx.id)
                    )
                  )
                  ioTxVertex.setProperty(ioTransactionSchema.links.head.propertyName, headerVertex.getId)
                  graph.addEdge(s"class:${addressTxoEdge.name}", lockAddressVertex, txoVertex, addressTxoEdge.label)

                }

                ioTx.groupPolicies.map(_.event).map { policy =>
                  if (graph.getGroupPolicy(policy.computeId).isEmpty)
                    graph.addGroupPolicy(policy)
                }

                ioTx.seriesPolicies.map(_.event).map { policy =>
                  if (graph.getSeriesPolicy(policy.computeId).isEmpty)
                    graph.addSeriesPolicy(policy)
                }

              }

              // Relationships between Header <-> TxIOs
              block.body.transactions.foreach(insertTx(_, isReward = false))
              block.body.rewardTransaction.foreach(insertTx(_, isReward = true))

              // Relationship between Header <-> ParentHeader if Not Genesis block
              if (block.header.height != 1) {
                val headerInVertex = graph
                  .getVertices(Field.BlockId, block.header.parentHeaderId.value.toByteArray)
                  .iterator()
                  .next()

                graph.addEdge(s"class:${blockHeaderEdge.name}", headerVertex, headerInVertex, blockHeaderEdge.label)
              }

              graph.commit()
            }.toEither
              .leftMap { th =>
                graph.rollback()
                GEs.InternalMessage(th.getMessage): GE
              }
          }

        override def remove(block: BlockData): F[Either[GE, Unit]] =
          OrientThread[F].delay {
            Try {
              val txosToRemove = block.body.allTransactions
                .flatMap { ioTx =>
                  ioTx.outputs.zipWithIndex.map { case (utxo, index) =>
                    TransactionOutputAddress(utxo.address.network, utxo.address.ledger, index, ioTx.id)
                  }
                }
                .map(graph.getTxo)

              graph.getBlockHeader(block.header).foreach { headerVertex =>
                (
                  Seq(graph.getBody(headerVertex)) ++
                  graph.getIoTxs(headerVertex).map(_.some) ++
                  txosToRemove :+
                  headerVertex.some
                ).flatten.map(graph.removeVertex)
              }

              // Lookup previous unspent TXOs, and update the state
              block.body.allTransactions.foreach(ioTx =>
                ioTx.inputs.foreach { spentTransactionOutput =>
                  graph
                    .getTxo(spentTransactionOutput.address)
                    .foreach { vertex =>
                      vertex.setProperty(SchemaTxo.Field.State, TxoState.UNSPENT.value)
                      vertex.setProperty(SchemaTxo.Field.SpendingTransaction, null)
                      vertex.setProperty(SchemaTxo.Field.SpendingInputIndex, null)
                    }
                }
              )

              block.body.allTransactions.flatMap(_.groupPolicies).map(_.event).foreach { policy =>
                graph.getGroupPolicy(policy.computeId).foreach(graph.removeVertex)
              }
              block.body.allTransactions.flatMap(_.seriesPolicies).map(_.event).foreach { policy =>
                graph.getSeriesPolicy(policy.computeId).foreach(graph.removeVertex)
              }

              graph.commit()
            }.toEither
              .leftMap { th =>
                graph.rollback()
                GEs.InternalMessage(th.getMessage): GE
              }
          }

      }
    )

}
