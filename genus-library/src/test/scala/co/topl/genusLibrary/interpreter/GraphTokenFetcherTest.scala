package co.topl.genusLibrary.interpreter

import cats.effect.IO
import cats.implicits._
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.{GroupId, SeriesId, TransactionOutputAddress}
import co.topl.genusLibrary.algebras.VertexFetcherAlgebra
import co.topl.genusLibrary.model.{GE, GEs}
import co.topl.genusLibrary.orientDb.instances.{SchemaGroupPolicy, SchemaSeriesPolicy}
import com.tinkerpop.blueprints.Vertex
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class GraphTokenFetcherTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("On fetchGroupPolicy with throwable response, a FailureMessageWithCause should be returned") {

    PropF.forAllF { (groupId: GroupId) =>
      withMock {

        val res = for {
          vertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          expectedTh = new IllegalStateException("boom!")
          _ = (vertexFetcher.fetchGroupPolicy _)
            .expects(groupId)
            .once()
            .returning(
              (GEs
                .InternalMessageCause("GraphVertexFetcher:fetchGroupPolicy", expectedTh): GE)
                .asLeft[Option[Vertex]]
                .pure[F]
            )
          tokenFetcher <- GraphTokenFetcher.make[F](vertexFetcher)
          _ <- assertIO(
            tokenFetcher.fetchGroupPolicy(groupId),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchGroupPolicy", expectedTh): GE)
              .asLeft[Option[GroupPolicy]]
          ).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchGroupPolicy if the Vertex exist, Some GroupPolicy should be returned") {

    PropF.forAllF { (groupId: GroupId, seriesId: SeriesId, address: TransactionOutputAddress) =>
      withMock {

        val res = for {
          vertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          vertex        <- mock[Vertex].pure[F].toResource

          groupPolicy = GroupPolicy("fooboo", address, Some(seriesId))

          _ = (vertexFetcher.fetchGroupPolicy _)
            .expects(groupId)
            .once()
            .returning(Option(vertex).asRight[GE].pure[F])

          _ = (vertex.getProperty[String] _)
            .expects(SchemaGroupPolicy.Field.Label)
            .once()
            .returning(groupPolicy.label)

          _ = (vertex.getProperty[Array[Byte]] _)
            .expects(SchemaGroupPolicy.Field.RegistrationUtxo)
            .once()
            .returning(groupPolicy.registrationUtxo.toByteArray)

          _ = (vertex.getProperty[Array[Byte]] _)
            .expects(SchemaGroupPolicy.Field.FixedSeries)
            .once()
            .returning(groupPolicy.fixedSeries.map(_.value.toByteArray).getOrElse(Array.empty[Byte]))

          tokenFetcher <- GraphTokenFetcher.make[F](vertexFetcher)
          _            <- assertIO(tokenFetcher.fetchGroupPolicy(groupId), Some(groupPolicy).asRight[GE]).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchSeriesPolicy with throwable response, a FailureMessageWithCause should be returned") {

    PropF.forAllF { (id: SeriesId) =>
      withMock {

        val res = for {
          vertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          expectedTh = new IllegalStateException("boom!")
          _ = (vertexFetcher.fetchSeriesPolicy _)
            .expects(id)
            .once()
            .returning(
              (GEs
                .InternalMessageCause("GraphVertexFetcher:fetchGroupPolicy", expectedTh): GE)
                .asLeft[Option[Vertex]]
                .pure[F]
            )
          tokenFetcher <- GraphTokenFetcher.make[F](vertexFetcher)
          _ <- assertIO(
            tokenFetcher.fetchSeriesPolicy(id),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchGroupPolicy", expectedTh): GE)
              .asLeft[Option[SeriesPolicy]]
          ).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchSeriesPolicy if the Vertex exist, Some SeriesPolicy should be returned") {

    PropF.forAllF { (id: SeriesId, address: TransactionOutputAddress) =>
      withMock {

        val res = for {
          vertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          vertex        <- mock[Vertex].pure[F].toResource

          seriesPolicy = SeriesPolicy(
            label = "fooboo",
            tokenSupply = Some(1),
            registrationUtxo = address
          )

          _ = (vertexFetcher.fetchSeriesPolicy _)
            .expects(id)
            .once()
            .returning(Option(vertex).asRight[GE].pure[F])

          _ = (vertex.getProperty[String] _)
            .expects(SchemaSeriesPolicy.Field.Label)
            .once()
            .returning(seriesPolicy.label)

          _ = (vertex.getProperty[Int] _)
            .expects(SchemaSeriesPolicy.Field.TokenSupply)
            .once()
            .returning(seriesPolicy.tokenSupply.get)

          _ = (vertex.getProperty[Array[Byte]] _)
            .expects(SchemaSeriesPolicy.Field.RegistrationUtxo)
            .once()
            .returning(seriesPolicy.registrationUtxo.toByteArray)

          _ = (vertex.getProperty[Int] _)
            .expects(SchemaSeriesPolicy.Field.QuantityDescriptor)
            .once()
            .returning(seriesPolicy.quantityDescriptor.value)

          _ = (vertex.getProperty[Int] _)
            .expects(SchemaSeriesPolicy.Field.Fungibility)
            .once()
            .returning(seriesPolicy.fungibility.value)

          _ = (vertex.getProperty[Array[Byte]] _)
            .expects(SchemaSeriesPolicy.Field.EphemeralMetadataScheme)
            .once()
            .returning(seriesPolicy.ephemeralMetadataScheme.map(_.toByteArray).getOrElse(Array.empty[Byte]))

          _ = (vertex.getProperty[Array[Byte]] _)
            .expects(SchemaSeriesPolicy.Field.PermanentMetadataScheme)
            .once()
            .returning(seriesPolicy.permanentMetadataScheme.map(_.toByteArray).getOrElse(Array.empty[Byte]))

          tokenFetcher <- GraphTokenFetcher.make[F](vertexFetcher)
          _            <- assertIO(tokenFetcher.fetchSeriesPolicy(id), Some(seriesPolicy).asRight[GE]).toResource
        } yield ()

        res.use_
      }

    }
  }

}
