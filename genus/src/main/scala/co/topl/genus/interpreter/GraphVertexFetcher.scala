package co.topl.genus.interpreter

import cats.effect.Resource
import cats.implicits._
import co.topl.brambl.models._
import co.topl.brambl.syntax.transactionIdAsIdSyntaxOps
import co.topl.consensus.models.BlockId
import co.topl.genus.algebras.VertexFetcherAlgebra
import co.topl.genus.model.{GE, GEs}
import co.topl.genus.orientDb.OrientThread
import co.topl.genus.orientDb.instances.SchemaIoTransaction.Field
import co.topl.genus.orientDb.instances.VertexSchemaInstances.instances._
import co.topl.genus.orientDb.instances._
import co.topl.genus.orientDb.schema.EdgeSchemaInstances
import co.topl.genus.services._
import com.orientechnologies.orient.core.sql.OCommandSQL
import com.orientechnologies.orient.core.sql.query.OSQLSynchQuery
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.{OrientBaseGraph, OrientDynaElementIterable, OrientVertex}

import scala.jdk.CollectionConverters._
import scala.util.Try

object GraphVertexFetcher {

  def make[F[_]: OrientThread](
    orientGraph: OrientBaseGraph
  ): Resource[F, VertexFetcherAlgebra[F]] =
    Resource.pure {
      new VertexFetcherAlgebra[F] {

        def fetchCanonicalHead(): F[Either[GE, Option[Vertex]]] =
          OrientThread[F].delay(
            Try(
              sqlVertices(
                s"SELECT FROM ${blockHeaderSchema.name} ORDER BY ${SchemaBlockHeader.Field.Height} DESC LIMIT 1"
              )(Nil).to(Iterable).headOption
            ).toEither
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchCanonicalHead", tx))
          )

        def fetchHeader(blockId: BlockId): F[Either[GE, Option[Vertex]]] =
          OrientThread[F].delay(
            Try(
              sqlVertices(
                s"SELECT FROM ${blockHeaderSchema.name} WHERE ${SchemaBlockHeader.Field.BlockId} = ? LIMIT 1"
              )(List(blockId.value.toByteArray)).to(Iterable).headOption
            ).toEither
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchHeader", tx))
          )

        def fetchHeaderByHeight(height: Long): F[Either[GE, Option[Vertex]]] =
          OrientThread[F].delay(
            Try(
              sqlVertices(
                s"SELECT FROM ${blockHeaderSchema.name} WHERE ${SchemaBlockHeader.Field.Height} = ? LIMIT 1"
              )(List(java.lang.Long.valueOf(height))).to(Iterable).headOption
            ).toEither
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
                .map(blockHeaderSchema.decode)
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
              sqlVertices(
                s"SELECT FROM ${blockBodySchema.name} WHERE ${SchemaBlockBody.Field.Header} = ? LIMIT 1"
              )(List(headerVertex)).to(Iterable).headOption
            ).toEither
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchBody", tx))
          )

        def fetchTransactions(headerVertex: Vertex): F[Either[GE, List[Vertex]]] =
          OrientThread[F].delay(
            Try(
              sqlVertices(
                s"SELECT FROM ${ioTransactionSchema.name} WHERE ${SchemaBlockHeader.Field.BlockId} = ? AND ${Field.IsReward} = false"
              )(List(headerVertex)).toList
            ).toEither
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchTransactions", tx))
          )

        def fetchRewardTransaction(headerVertex: Vertex): F[Either[GE, Option[Vertex]]] =
          OrientThread[F].delay(
            Try(
              sqlVertices(
                s"SELECT FROM ${ioTransactionSchema.name} WHERE ${SchemaBlockHeader.Field.BlockId} = ? AND ${Field.IsReward} = true LIMIT 1"
              )(List(headerVertex)).to(Iterable).headOption
            ).toEither
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchRewardTransaction", tx))
          )

        def fetchTransaction(ioTransaction32: TransactionId): F[Either[GE, Option[Vertex]]] =
          OrientThread[F].delay(
            Try(
              sqlVertices(
                s"SELECT FROM ${ioTransactionSchema.name} WHERE ${SchemaIoTransaction.Field.TransactionId} = ? LIMIT 1"
              )(List(ioTransaction32.id.value.toByteArray)).to(Iterable).headOption
            ).toEither
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchTransaction", tx))
          )

        // TODO Create method fetchLockAddress(lockId: LockId)
        def fetchLockAddress(lockAddress: LockAddress): F[Either[GE, Option[Vertex]]] =
          OrientThread[F].delay(
            Try(
              sqlVertices(
                s"SELECT FROM ${SchemaLockAddress.Field.SchemaName} WHERE ${SchemaLockAddress.Field.AddressId} = ? LIMIT 1"
              )(List(lockAddress.id.value.toByteArray)).to(Iterable).headOption
            ).toEither
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchLockAddress", tx))
          )

        def fetchTxo(transactionOutputAddress: TransactionOutputAddress): F[Either[GE, Option[Vertex]]] =
          OrientThread[F].delay(
            Try(
              sqlVertices(
                s"SELECT FROM ${SchemaTxo.Field.SchemaName} WHERE ${SchemaTxo.Field.TxoId} = ? LIMIT 1"
              )(List(transactionOutputAddress.id.value.toByteArray :+ transactionOutputAddress.index.byteValue))
                .to(Iterable)
                .headOption
            ).toEither
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchTxo", tx))
          )

        def fetchTxosByLockAddress(lockAddressVertex: Vertex, state: TxoState): F[Either[GE, List[Vertex]]] =
          OrientThread[F].delay(
            Try(
              sqlVertices(
                // Nested query:
                // Step 1: Select outbound edges
                // Step 2: Filter result by state
                s"SELECT FROM(" +
                s"SELECT expand(out('${EdgeSchemaInstances.addressTxoEdge.label}')) FROM ?" +
                s") WHERE ${SchemaTxo.Field.State} = ?"
              )(List(lockAddressVertex, java.lang.Integer.valueOf(state.value)))
                .to(List)
            ).toEither
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchTxosByLockAddress", tx))
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

        def fetchBlockStats(): F[Either[GE, BlockStats]] =
          OrientThread[F].delay(
            Try {
              val queryEmptyBlocks: java.lang.Iterable[OrientVertex] =
                orientGraph
                  .command(
                    new OSQLSynchQuery[OrientVertex](
                      "select count(*) as count from blockbody where transactionIds.size() = 0"
                    )
                  )
                  .execute()

              val queryNonEmptyBlocks: java.lang.Iterable[OrientVertex] =
                orientGraph
                  .command(
                    new OSQLSynchQuery[OrientVertex](
                      "select count(*) as count from blockbody where transactionIds.size() > 0"
                    )
                  )
                  .execute()

              queryEmptyBlocks.asScala.headOption
                .map(_.getProperty[java.lang.Number]("count"))
                .map(n => BlockStats.defaultInstance.withEmpty(n.longValue()))
                .flatMap { res =>
                  queryNonEmptyBlocks.asScala.headOption
                    .map(_.getProperty[java.lang.Number]("count"))
                    .map(n => res.withNonEmpty(n.longValue()))
                }
                .getOrElse(BlockStats.defaultInstance)

            }.toEither
              .leftMap[GE] { tx =>
                GEs.InternalMessageCause("GraphVertexFetcher:fetchBlockStats", tx)
              }
          )

        def fetchGroupPolicy(groupId: GroupId): F[Either[GE, Option[Vertex]]] =
          OrientThread[F].delay(
            Try(
              sqlVertices(
                s"SELECT FROM ${SchemaGroupPolicy.Field.SchemaName} WHERE ${SchemaGroupPolicy.Field.GroupPolicyId} = ? LIMIT 1"
              )(List(groupId.value.toByteArray))
                .to(Iterable)
                .headOption
            ).toEither
              .leftMap[GE](th => GEs.InternalMessageCause("GraphVertexFetcher:fetchGroupPolicy", th))
          )

        def fetchSeriesPolicy(seriesId: SeriesId): F[Either[GE, Option[Vertex]]] =
          OrientThread[F].delay(
            Try(
              sqlVertices(
                s"SELECT FROM ${SchemaSeriesPolicy.Field.SchemaName} WHERE ${SchemaSeriesPolicy.Field.SeriesPolicyId} = ? LIMIT 1"
              )(List(seriesId.value.toByteArray))
                .to(Iterable)
                .headOption
            ).toEither
              .leftMap[GE](th => GEs.InternalMessageCause("GraphVertexFetcher:fetchSeriesPolicy", th))
          )

        /**
         * Executes the given raw SQL using the given arguments
         * @param query raw SQL statement
         * @param args arguments to be substituted for `?` in the query
         * @return an iterator of OrientDB objects, i.e. OrientVertex
         */
        private def sqlIterator(query: String)(args: Seq[Object]): Iterator[Object] =
          orientGraph
            .command(new OCommandSQL(query))
            .execute[OrientDynaElementIterable](args: _*)
            .iterator()
            .asScala

        /**
         * Executes the given raw SQL using the given arguments, and extracts results as Vertex objects.
         * If possible, calls reload() on each OrientVertex object.
         * @param query raw SQL statement, expected to return Vertex objects
         * @param args arguments to be substituted for `?` in the query
         * @return an iterator of Vertex objects
         */
        private def sqlVertices(query: String)(args: Seq[Object]): Iterator[Vertex] =
          sqlIterator(query)(args)
            .map {
              case o: OrientVertex =>
                o.reload()
                o
              case o =>
                o
            }
            .collect { case v: Vertex @unchecked => v }

      }
    }
}
