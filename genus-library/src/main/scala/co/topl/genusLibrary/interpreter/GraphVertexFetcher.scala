package co.topl.genusLibrary.interpreter

import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.consensus.models.BlockId
import co.topl.genusLibrary.algebras.VertexFetcherAlgebra
import co.topl.genusLibrary.model.{GRE, GREs}
import co.topl.genusLibrary.orientDb.schema.SchemaBlockHeader.Field
import co.topl.genusLibrary.orientDb.schema.VertexSchemaInstances.instances._
import com.orientechnologies.orient.core.sql.query.OSQLSynchQuery
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.{OrientGraphCommand, OrientGraphNoTx, OrientVertex}
import scala.jdk.CollectionConverters._
import scala.util.Try

object GraphVertexFetcher {

  def make[F[_]: Async](orientGraph: OrientGraphNoTx): Resource[F, VertexFetcherAlgebra[F]] =
    Resource.pure {
      new VertexFetcherAlgebra[F] {

        override def fetchHeader(blockId: BlockId): F[Either[GRE, Option[Vertex]]] =
          Async[F].blocking(
            Try(orientGraph.getVertices(Field.BlockId, blockId.value.toByteArray).asScala).toEither
              .map(_.headOption)
              .leftMap[GRE](tx => GREs.MessageCause("GraphVertexFetcher:fetchHeader", tx))
          )

        def fetchHeaderByHeight(height: Long): F[Either[GRE, Option[Vertex]]] =
          Async[F].blocking(
            Try(
              orientGraph.getVertices(blockHeaderSchema.name, Array(Field.Height), Array(height)).asScala
            ).toEither
              .map(_.headOption)
              .leftMap[GRE](tx => GREs.MessageCause("GraphVertexFetcher:fetchHeaderByHeight", tx))
          )

        def fetchHeaderByDepth(depth: Long): F[Either[GRE, Option[Vertex]]] =
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

              // works
//              val query: java.lang.Iterable[OrientVertex] =
//                orientGraph.command(new OSQLSynchQuery[OrientVertex](queryString)).execute()

              // works
              val query2: java.lang.Iterable[OrientVertex] =
                new OrientGraphCommand(orientGraph, new OSQLSynchQuery[OrientVertex](queryString))
                  .execute()

              query2.asScala.headOption
                .map(blockHeaderSchema.decodeVertex)
                .map(_.height)
                .map(_ - depth)
                .map(height =>
                  orientGraph.getVertices(blockHeaderSchema.name, Array(Field.Height), Array(height)).asScala
                )
                .flatMap(_.headOption)

            }.toEither
              .leftMap[GRE](tx => GREs.MessageCause("GraphVertexFetcher:fetchHeaderByDepth", tx))
          )

        override def fetchBody(headerVertex: Vertex): F[Either[GRE, Option[Vertex]]] =
          Async[F].blocking(
            Try(
              orientGraph
                .getVertices(blockBodySchema.name, Array(Field.BlockId), Array(headerVertex.getId))
                .asScala
            ).toEither
              .map(_.headOption)
              .leftMap[GRE](tx => GREs.MessageCause("GraphVertexFetcher:fetchBody", tx))
          )

        override def fetchTransactions(headerVertex: Vertex): F[Either[GRE, Iterable[Vertex]]] =
          Async[F].blocking(
            Try(
              orientGraph
                .getVertices(ioTransactionSchema.name, Array(Field.BlockId), Array(headerVertex.getId))
                .asScala
            ).toEither
              .leftMap[GRE](tx => GREs.MessageCause("GraphVertexFetcher:fetchTransactions", tx))
          )

      }
    }
}
