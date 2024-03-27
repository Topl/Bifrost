package co.topl.genus.interpreter

import cats.data.EitherT
import cats.effect._
import cats.implicits._
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax._
import co.topl.genus.algebras._
import co.topl.genus.model.{GE, GEs}
import co.topl.genus.orientDb.OrientThread
import co.topl.genus.orientDb.instances.VertexSchemaInstances.implicits._
import co.topl.genus.orientDb.instances._
import co.topl.genus.orientDb.schema.EdgeSchemaInstances._
import co.topl.genus.services.{BlockData, Txo, TxoState}
import co.topl.models.utility._
import co.topl.node.models.BlockBody
import co.topl.typeclasses.implicits._
import com.orientechnologies.orient.core.sql.OCommandSQL
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.{OrientDynaElementIterable, OrientGraph}
import fs2.Stream
import org.typelevel.log4cats.Logger

import scala.jdk.CollectionConverters._
import scala.util.Try

object GraphBlockUpdater {

  def make[F[_]: OrientThread: Sync: Logger](
    graph:            OrientGraph,
    blockFetcher:     BlockFetcherAlgebra[F],
    nodeBlockFetcher: NodeBlockFetcherAlgebra[F, Stream[F, *]]
  ): Resource[F, BlockUpdaterAlgebra[F]] =
    Resource.pure(
      new BlockUpdaterAlgebra[F] {

        private val syncLogger = org.slf4j.LoggerFactory.getLogger("GraphBlockUpdater")

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
          Sync[F]
            .delay(BlockBody(block.body.transactions.map(_.id), block.body.rewardTransaction.map(_.id)))
            .flatMap(body =>
              Sync[F].uncancelable(_ =>
                OrientThread[F].delay {
                  Try {
                    val headerVertex = graph.addBlockHeader(block.header)

                    // Relationships between Header <-> Body
                    val bodyVertex = graph.addBody(body)
                    bodyVertex.setProperty(blockBodySchema.links.head.propertyName, headerVertex)
                    graph
                      .addEdge(
                        s"class:${blockHeaderBodyEdge.name}",
                        headerVertex,
                        bodyVertex,
                        blockHeaderBodyEdge.label
                      )

                    def insertTx(ioTx: IoTransaction, isReward: Boolean): Unit = {
                      val ioTxVertex = graph.addIoTx(ioTx)
                      ioTxVertex.setProperty(SchemaIoTransaction.Field.ParentBlock, headerVertex)
                      ioTxVertex.setProperty(SchemaIoTransaction.Field.IsReward, isReward)
                      if (isReward) {
                        headerVertex.addEdge(blockHeaderRewardEdge.label, ioTxVertex)
                      } else {
                        headerVertex.addEdge(blockHeaderTxIOEdge.label, ioTxVertex)

                        // Lookup previous unspent TXOs, and update the state
                        ioTx.inputs.zipWithIndex.foreach { case (spentTransactionOutput, inputIndex) =>
                          graph
                            .getTxo(spentTransactionOutput.address) match {
                            case Some(vertex) =>
                              vertex.setProperty(SchemaTxo.Field.State, java.lang.Integer.valueOf(TxoState.SPENT.value))
                              vertex.setProperty(SchemaTxo.Field.SpendingTransaction, ioTxVertex)
                              vertex
                                .setProperty(SchemaTxo.Field.SpendingInputIndex, java.lang.Integer.valueOf(inputIndex))
                            case _ =>
                              syncLogger.warn(show"Missing expected TxO address=${spentTransactionOutput.address}")
                          }
                        }
                      }

                      // Relationships between TxIOs <-> LockAddress
                      ioTx.outputs.zipWithIndex.foreach { case (utxo, index) =>
                        // before adding a new address, check if was not there included by a previous transaction
                        val lockAddressVertex = {
                          val addressIterator =
                            graph
                              .getVertices(SchemaLockAddress.Field.AddressId, utxo.address.id.value.toByteArray)
                              .iterator()

                          if (addressIterator.hasNext) addressIterator.next()
                          else graph.addLockAddress(utxo.address)

                        }
                        lockAddressVertex.addEdge(addressTxIOEdge.label, ioTxVertex)

                        val txoVertex = graph.addTxo(
                          Txo(
                            utxo,
                            TxoState.UNSPENT,
                            TransactionOutputAddress(utxo.address.network, utxo.address.ledger, index, ioTx.id)
                          )
                        )
                        lockAddressVertex.addEdge(addressTxoEdge.label, txoVertex)
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
                        .command(
                          new OCommandSQL(
                            s"SELECT FROM ${SchemaBlockHeader.SchemaName} WHERE ${SchemaBlockHeader.Field.BlockId} = ? LIMIT 1"
                          )
                        )
                        .execute[OrientDynaElementIterable](block.header.parentHeaderId.value.toByteArray)
                        .iterator()
                        .asScala
                        .collectFirst { case v: Vertex @unchecked => v }
                        .get

                      graph
                        .addEdge(s"class:${blockHeaderEdge.name}", headerVertex, headerInVertex, blockHeaderEdge.label)
                    }

                    graph.commit()
                  }.toEither
                    .leftMap { th =>
                      graph.rollback()
                      GEs.InternalMessage(th.getMessage): GE
                    }
                }
              )
            )

        override def remove(block: BlockData): F[Either[GE, Unit]] =
          Sync[F].uncancelable(_ =>
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
                        vertex.setProperty(SchemaTxo.Field.State, java.lang.Integer.valueOf(TxoState.UNSPENT.value))
                        vertex.removeProperty[Object](SchemaTxo.Field.SpendingTransaction)
                        vertex.removeProperty[Object](SchemaTxo.Field.SpendingInputIndex)
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
          )

      }
    )

}
