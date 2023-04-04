package co.topl.genusLibrary.interpreter

import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.brambl.models.Identifier
import co.topl.brambl.syntax.transactionIdAsIdSyntaxOps
import co.topl.consensus.models.BlockId
import co.topl.genusLibrary.algebras.VertexFetcherAlgebra
import co.topl.genusLibrary.model.{GE, GEs}
import co.topl.genusLibrary.orientDb.instances.{SchemaAddress, SchemaBlockHeader, SchemaIoTransaction}
import co.topl.genusLibrary.orientDb.instances.VertexSchemaInstances.instances._
import com.orientechnologies.orient.core.sql.query.OSQLSynchQuery
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.{OrientGraphNoTx, OrientVertex}
import scala.jdk.CollectionConverters._
import scala.util.Try

object GraphVertexFetcher {

  def make[F[_]: Async](orientGraph: OrientGraphNoTx): Resource[F, VertexFetcherAlgebra[F]] =
    Resource.pure {
      new VertexFetcherAlgebra[F] {

        override def fetchHeader(blockId: BlockId): F[Either[GE, Option[Vertex]]] =
          Async[F].blocking(
            Try(orientGraph.getVertices(SchemaBlockHeader.Field.BlockId, blockId.value.toByteArray).asScala).toEither
              .map(_.headOption)
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchHeader", tx))
          )

        def fetchHeaderByHeight(height: Long): F[Either[GE, Option[Vertex]]] =
          Async[F].blocking(
            Try(
              orientGraph
                .getVertices(blockHeaderSchema.name, Array(SchemaBlockHeader.Field.Height), Array(height))
                .asScala
            ).toEither
              .map(_.headOption)
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchHeaderByHeight", tx))
          )

        def fetchHeaderByDepth(depth: Long): F[Either[GE, Option[Vertex]]] =
          Async[F].blocking(
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

        override def fetchBody(headerVertex: Vertex): F[Either[GE, Option[Vertex]]] =
          Async[F].blocking(
            Try(
              orientGraph
                .getVertices(blockBodySchema.name, Array(SchemaBlockHeader.Field.BlockId), Array(headerVertex.getId))
                .asScala
            ).toEither
              .map(_.headOption)
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchBody", tx))
          )

        override def fetchTransactions(headerVertex: Vertex): F[Either[GE, Iterable[Vertex]]] =
          Async[F].blocking(
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

        def fetchTransaction(ioTransaction32: Identifier.IoTransaction32): F[Either[GE, Option[Vertex]]] =
          Async[F].blocking(
            Try(
              orientGraph
                .getVertices(
                  SchemaIoTransaction.Field.TransactionId,
                  ioTransaction32.id.evidence.digest.value.toByteArray
                )
                .asScala
            ).toEither
              .map(_.headOption)
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchTransaction", tx))
          )

        def fetchAddress(addressId: Identifier): F[Either[GE, Option[Vertex]]] =
          Async[F].blocking(
            Try(
              orientGraph
                .getVertices(
                  SchemaAddress.Field.AddressId,
                  addressId.toByteArray
                )
                .asScala
            ).toEither
              .map(_.headOption)
              .leftMap[GE](tx => GEs.InternalMessageCause("GraphVertexFetcher:fetchAddress", tx))
          )

      }
    }
}
