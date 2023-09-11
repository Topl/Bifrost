package co.topl.genusLibrary.interpreter

import cats.effect.IO
import cats.implicits._
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models.Event.GroupPolicy
import co.topl.brambl.models.{GroupId, SeriesId, TransactionOutputAddress}
import co.topl.genusLibrary.algebras.VertexFetcherAlgebra
import co.topl.genusLibrary.model.{GE, GEs}
import co.topl.genusLibrary.orientDb.instances.SchemaGroupPolicy
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

          _ = (vertex.getPropertyKeys _)
            .expects()
            .once()
            .returning(
              java.util.Set.of(
                SchemaGroupPolicy.Field.Label,
                SchemaGroupPolicy.Field.RegistrationUtxo,
                SchemaGroupPolicy.Field.FixedSeries
              )
            )

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

}
