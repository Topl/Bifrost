package co.topl.genusLibrary.orientDb

import cats.effect.IO
import cats.implicits._
import co.topl.genusLibrary.failure.{Failure, Failures}
import co.topl.genusLibrary.orientDb.wrapper.{GraphTxWrapper, WrappedEdge, WrappedVertex}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.immutable

class GraphTxDAOSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  type F[A] = IO[A]

  private val wrappedGraph = mock[GraphTxWrapper]

  private val graphTxDao = new GraphTxDAO[F](wrappedGraph)

  test("On a transaction with a failing function return, no methods on the graph are executed") {
    withMock {

      val leftFailure = mock[Failure].asLeft[Any]

      val request = leftFailure.pure[F]

      val response = graphTxDao.withEffectfulTransaction(_ => request)

      assertIO(
        response,
        leftFailure
      )

    }
  }

  test("On a transaction with a successful function return and failing commit, a rollback should be executed") {
    withMock {

      val request = 2.asRight[Failure].pure[F]

      inSequence {

        val ex = new RuntimeException()

        (wrappedGraph.commit _)
          .expects()
          .throws(ex)

        (wrappedGraph.rollback _)
          .expects()
          .returns(())
          .once()

        val response = graphTxDao.withEffectfulTransaction(_ => request)

        assertIO(
          response,
          Failures.OrientCommitException(ex).asLeft
        )
      }

    }
  }

  test("On a transaction with a successful function return and successful commit, no rollback should be executed") {
    withMock {

      val request = 2.asRight[Failure].pure[F]

      (wrappedGraph.commit _)
        .expects()
        .returns(())
        .once()

      val response = graphTxDao.withEffectfulTransaction(_ => request)

      assertIO(
        response,
        2.asRight
      )

    }
  }

  test("Vertex creation should create a vertex with all the mapped fields being passed") {
    withMock {
      case class Foo(bar: Int)

      trait VertexSchemaForFoo extends VertexSchema[Foo]

      implicit val vertexSchema: VertexSchema[Foo] = mock[VertexSchemaForFoo]

      val fooVertex = mock[WrappedVertex]

      val elem = Foo(2)

      (() => vertexSchema.name)
        .expects()
        .returns("FooClassName")
        .once()

      (wrappedGraph.addVertex _)
        .expects("class:FooClassName")
        .returns(fooVertex)
        .once()

      (vertexSchema.encode _)
        .expects(elem)
        .returns(immutable.Map[String, AnyRef]("bar" -> "john_doe"))
        .once()

      (fooVertex
        .setProperty(_: String, _: AnyRef))
        .expects("bar", "john_doe")
        .returns()
        .once()

      val response = graphTxDao.createVertex(elem)

      IO {
        assertEquals(response, (elem, fooVertex))
      }

    }

  }

  test("Edge creation should create a edge with all the mapped fields being passed") {

    withMock {

      val outVertex = mock[WrappedVertex]
      val inVertex = mock[WrappedVertex]
      val label = Some("label")

      val edge = mock[WrappedEdge]

      (wrappedGraph
        .addEdge(_: AnyRef, _: WrappedVertex, _: WrappedVertex, _: String))
        .expects(null, outVertex, inVertex, "label")
        .returns(edge)
        .once()

      val response = graphTxDao.addEdge(outVertex, inVertex, label)

      IO(assertEquals(response, edge))

    }

  }

}
