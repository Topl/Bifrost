package co.topl.genusLibrary.interpreter

import cats.effect.Resource
import cats.implicits._
import co.topl.brambl.models.{LockAddress, TransactionId, TransactionOutputAddress}
import co.topl.brambl.syntax.transactionIdAsIdSyntaxOps
import co.topl.consensus.models.BlockId
import co.topl.genus.services._
import co.topl.genusLibrary.algebras.VertexFetcherAlgebra
import co.topl.genusLibrary.model.{GE, GEs}
import co.topl.genusLibrary.orientDb.OrientThread
import co.topl.genusLibrary.orientDb.instances.VertexSchemaInstances.instances._
import co.topl.genusLibrary.orientDb.instances.{SchemaBlockHeader, SchemaIoTransaction, SchemaLockAddress, SchemaTxo}
import com.orientechnologies.orient.core.sql.query.OSQLSynchQuery
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.{OrientGraphNoTx, OrientVertex}
import scala.jdk.CollectionConverters._
import scala.util.Try

object GraphVertexFetcher {

  def make[F[_]: OrientThread](
    orientGraph: OrientGraphNoTx
  ): Resource[F, VertexFetcherAlgebra[F]] =
    Resource.pure {
      new VertexFetcherAlgebra[F] {

        def fetchCanonicalHead(): F[Either[GE, Option[Vertex]]] =
          OrientThread[F].delay(
            Try(orientGraph.getVerticesOfClass(s"${canonicalHeadSchema.name}").asScala).toEither
              .map(_.headOption)
              .map(_.map(_.getProperty[OrientVertex](SchemaBlockHeader.Field.BlockId)))
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchCanonicalHead", tx))
          )

        def fetchHeader(blockId: BlockId): F[Either[GE, Option[Vertex]]] =
          OrientThread[F].delay(
            Try(orientGraph.getVertices(SchemaBlockHeader.Field.BlockId, blockId.value.toByteArray).asScala).toEither
              .map(_.headOption)
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchHeader", tx))
          )

        def fetchHeaderByHeight(height: Long): F[Either[GE, Option[Vertex]]] =
          OrientThread[F].delay(
            Try(
              orientGraph
                .getVertices(blockHeaderSchema.name, Array(SchemaBlockHeader.Field.Height), Array(height))
                .asScala
            ).toEither
              .map(_.headOption)
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchHeaderByHeight", tx))
          )

        def fetchHeaderByDepth(depth: Long): F[Either[GE, Option[Vertex]]] =
          OrientThread[F].delay(
            Try {

              /**
               * TODO For some reason the below works query works in the console, but now throw the api, check it out later
               * Alternative code works, doind 2 round trips to the database, which should be fixed eventually
               * val queryString = s"select expand($$res) let $$block_depth = first((select (height- 5) as depth from BlockHeader order by height desc limit 1)), $$res = (select from BlockHeader where height = $$block_depth.depth)"
               * val params = Map(("depthParam", depth)).asJava
               * orientGraph.command(new OSQLSynchQuery[OrientVertex](queryString)).execute(params)
               */
              val queryString = s"select from blockHeader order by height desc limit 1"

              val query: java.lang.Iterable[OrientVertex] =
                orientGraph.command(new OSQLSynchQuery[OrientVertex](queryString)).execute()

              query.asScala.headOption
                .map(blockHeaderSchema.decodeVertex)
                .map(_.height)
                .map(_ - depth)
                .map(height =>
                  orientGraph
                    .getVertices(blockHeaderSchema.name, Array(SchemaBlockHeader.Field.Height), Array(height))
                    .asScala
                )
                .flatMap(_.headOption)

            }.toEither
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchHeaderByDepth", tx))
          )

        def fetchBody(headerVertex: Vertex): F[Either[GE, Option[Vertex]]] =
          OrientThread[F].delay(
            Try(
              orientGraph
                .getVertices(blockBodySchema.name, Array(SchemaBlockHeader.Field.BlockId), Array(headerVertex.getId))
                .asScala
            ).toEither
              .map(_.headOption)
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchBody", tx))
          )

        def fetchTransactions(headerVertex: Vertex): F[Either[GE, Iterable[Vertex]]] =
          OrientThread[F].delay(
            Try(
              orientGraph
                .getVertices(
                  ioTransactionSchema.name,
                  Array(SchemaBlockHeader.Field.BlockId),
                  Array(headerVertex.getId)
                )
                .asScala
            ).toEither
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchTransactions", tx))
          )

        def fetchTransaction(ioTransaction32: TransactionId): F[Either[GE, Option[Vertex]]] =
          OrientThread[F].delay(
            Try(
              orientGraph
                .getVertices(
                  SchemaIoTransaction.Field.TransactionId,
                  ioTransaction32.id.value.toByteArray
                )
                .asScala
            ).toEither
              .map(_.headOption)
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchTransaction", tx))
          )

        // TODO Create method fetchLockAddress(lockId: LockId)
        def fetchLockAddress(lockAddress: LockAddress): F[Either[GE, Option[Vertex]]] =
          OrientThread[F].delay(
            Try(
              orientGraph
                .getVertices(
                  SchemaLockAddress.Field.AddressId,
                  lockAddress.id.value.toByteArray
                )
                .asScala
            ).toEither
              .map(_.headOption)
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchLockAddress", tx))
          )

        def fetchTxo(transactionOutputAddress: TransactionOutputAddress): F[Either[GE, Option[Vertex]]] =
          OrientThread[F].delay(
            Try(
              orientGraph
                .getVertices(
                  SchemaTxo.Field.TxoId,
                  transactionOutputAddress.id.value.toByteArray :+ transactionOutputAddress.index.byteValue
                )
                .asScala
            ).toEither
              .map(_.headOption)
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchTxo", tx))
          )

        def fetchTxoStats(): F[Either[GE, TxoStats]] =
          OrientThread[F].delay(
            Try {

              val queryString = s"select count(state) as count, state from Txo group by state"

              val query: java.lang.Iterable[OrientVertex] =
                orientGraph.command(new OSQLSynchQuery[OrientVertex](queryString)).execute()

              query.asScala
                .map { v =>
                  v.getProperty[java.lang.Number]("state") -> v.getProperty[java.lang.Number]("count")
                }
                .foldLeft(TxoStats.defaultInstance) { case (stats, (state, count)) =>
                  TxoState.fromValue(state.intValue()) match {
                    case TxoState.SPENT => stats.withSpent(count.intValue()).withTotal(stats.total + count.intValue())
                    case TxoState.UNSPENT =>
                      stats.withUnspent(count.intValue()).withTotal(stats.total + count.intValue())
                    case TxoState.PENDING =>
                      stats.withPending(count.intValue()).withTotal(stats.total + count.intValue())
                    case _ => stats
                  }
                }

            }.toEither
              .leftMap[GE] { tx =>
                GEs.InternalMessageCause("GraphVertexFetcher:fetchTxoStats", tx)
              }
          )

        def fetchBlockchainSizeStats(): F[Either[GE, BlockchainSizeStats]] =
          OrientThread[F].delay(
            Try {

              val querySumBlockheader: java.lang.Iterable[OrientVertex] =
                orientGraph
                  .command(new OSQLSynchQuery[OrientVertex]("select sum(size) as sum from blockheader"))
                  .execute()

              val querySumTransaction: java.lang.Iterable[OrientVertex] =
                orientGraph
                  .command(new OSQLSynchQuery[OrientVertex]("select sum(size) as sum from transaction"))
                  .execute()

              querySumBlockheader.asScala.headOption
                .map(_.getProperty[java.lang.Number]("sum"))
                .map(n => BlockchainSizeStats.defaultInstance.withBlockHeaderBytes(n.longValue()))
                .flatMap { res =>
                  querySumTransaction.asScala.headOption
                    .map(_.getProperty[java.lang.Number]("sum"))
                    .map(n => res.withTransactionBytes(n.longValue()))
                }
                .getOrElse(BlockchainSizeStats.defaultInstance)

            }.toEither
              .leftMap[GE] { tx =>
                GEs.InternalMessageCause("GraphVertexFetcher:fetchBlockchainSizeStats", tx)
              }
          )
      }
    }
}
