package co.topl.genusLibrary.interpreter

import cats.effect.{IO, Resource}
import cats.effect.implicits.effectResourceOps
import cats.implicits._
import co.topl.consensus.models.BlockHeader
import co.topl.genus.services.BlockData
import co.topl.genusLibrary.model.{GE, GEs}
import co.topl.models.generators.consensus.ModelGenerators._
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.{OrientEdge, OrientGraph, OrientVertex}
import java.lang
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.annotation.nowarn
import scala.jdk.CollectionConverters._

class GraphBlockInserterSuite extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  test("Insert genesis block, should fail, if we can not add the vertex") {
    val orientGraph: OrientGraph = new OrientGraph("memory:test") {
      override def addVertex(id: Object, prop: AnyRef*): OrientVertex = throw new IllegalStateException("boom!")
    }

    PropF.forAllF { blockHeader: BlockHeader =>
      val res = for {
        blockData     <- Resource.pure(BlockData(blockHeader.copy(height = 1), null, null))
        blockInserter <- GraphBlockInserter.make[F](orientGraph)
        _ <- assertIO(
          blockInserter.insert(blockData),
          (GEs.InternalMessage("boom!"): GE).asLeft[Unit]
        ).toResource
      } yield ()
      res.use_
    }
    orientGraph.shutdown()
  }

  test("Insert genesis block, should work, if we add the vertex") {

    val orientGraph: OrientGraph = new OrientGraph("memory:test") {
      @nowarn
      override def addVertex(id: Object, prop: AnyRef*): OrientVertex = new OrientVertex()
    }

    PropF.forAllF { blockHeader: BlockHeader =>
      val res = for {
        blockData     <- Resource.pure(BlockData(blockHeader.copy(height = 1), null, null))
        blockInserter <- GraphBlockInserter.make[F](orientGraph)
        _ <- assertIO(
          blockInserter.insert(blockData),
          ().asRight[GE]
        ).toResource
      } yield ()

      res.use_

    }
    orientGraph.shutdown()
  }

  test("Insert no genesis block, should fail, if we can not add the vertex") {
    val orientGraph: OrientGraph = new OrientGraph("memory:test") {
      override def addVertex(id: Object, prop: AnyRef*): OrientVertex = throw new IllegalStateException("boom!")
    }
    PropF.forAllF { blockHeader: BlockHeader =>
      withMock {
        val res = for {
          blockData     <- Resource.pure(BlockData(blockHeader.copy(height = 2), null, null))
          blockInserter <- GraphBlockInserter.make[F](orientGraph)
          _ <- assertIO(
            blockInserter.insert(blockData),
            (GEs.InternalMessage("boom!"): GE).asLeft[Unit]
          ).toResource
        } yield ()
        res.use_

      }
    }
    orientGraph.shutdown()
  }

  test("Insert no genesis block, should work, if we add the vertex and the edge") {

    val orientGraph: OrientGraph = new OrientGraph("memory:test") {
      @nowarn
      override def addVertex(id: Object, prop: AnyRef*): OrientVertex = new OrientVertex()

      override def getVertices(id: String, prop: Object): lang.Iterable[Vertex] =
        Iterable(new OrientVertex(): Vertex).asJava

      override def addEdge(id: Object, o: Vertex, i: Vertex, l: String): OrientEdge =
        new OrientEdge()
    }

    PropF.forAllF { blockHeader: BlockHeader =>
      withMock {
        val res = for {
          blockData     <- Resource.pure(BlockData(blockHeader.copy(height = 2), null, null))
          blockInserter <- GraphBlockInserter.make[F](orientGraph)
          _ <- assertIO(
            blockInserter.insert(blockData),
            ().asRight[GE]
          ).toResource
        } yield ()
        res.use_
      }
    }
    orientGraph.shutdown()
  }

  test("Insert no genesis block, should fail, if we add the vertex but no the edge") {
    val orientGraph: OrientGraph = new OrientGraph("memory:test") {
      @nowarn
      override def addVertex(id: Object, prop: AnyRef*): OrientVertex = new OrientVertex()

      override def getVertices(id: String, prop: Object): lang.Iterable[Vertex] =
        Iterable(new OrientVertex(): Vertex).asJava

      override def addEdge(id: Object, o: Vertex, i: Vertex, l: String): OrientEdge =
        throw new IllegalStateException("boom!")
    }

    PropF.forAllF { blockHeader: BlockHeader =>
      withMock {
        val res = for {
          blockData     <- Resource.pure(BlockData(blockHeader.copy(height = 2), null, null))
          blockInserter <- GraphBlockInserter.make[F](orientGraph)
          _ <- assertIO(
            blockInserter.insert(blockData),
            (GEs.InternalMessage("boom!"): GE).asLeft[Unit]
          ).toResource
        } yield ()
        res.use_

      }
    }
    orientGraph.shutdown()
  }
}
