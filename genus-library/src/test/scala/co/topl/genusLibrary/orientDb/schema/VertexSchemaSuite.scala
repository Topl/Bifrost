package co.topl.genusLibrary.orientDb.schema

import cats.effect.implicits.effectResourceOps
import cats.effect.kernel.Async
import cats.effect.{IO, Resource, Sync}
import cats.implicits._
import co.topl.genusLibrary.orientDb.OrientDBMetadataFactory
import co.topl.genusLibrary.orientDb.schema.OTyped.Instances._
import com.tinkerpop.blueprints.impls.orient.OrientGraphFactory
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.jdk.CollectionConverters._

class VertexSchemaSuite extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]
  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  val StringParamName = "strParam"
  case class TestClass(strParam: String)

  // TODO
  // create 1 field for each OTyped Instance. Test them
  // create index, links. Test them
  private val testSchema: VertexSchema[TestClass] = VertexSchema.create(
    "SchemaNameTest",
    GraphDataEncoder[TestClass]
      .withProperty(StringParamName, _.strParam, mandatory = true, readOnly = true, notNull = false),
    v => TestClass(v(StringParamName): String)
  )

  test("Test Schema") {

    val g: OrientGraphFactory = new OrientGraphFactory("memory:test")

    val res = for {
      orientGraphFactory <- Resource.make[F, OrientGraphFactory](Sync[F].blocking(g))(g => Sync[F].delay(g.close()))
      db <- Resource.make(Sync[F].blocking(orientGraphFactory.getDatabase))(db => Sync[F].delay(db.close()))
      _  <- OrientDBMetadataFactory.createVertex[F](db, testSchema)

      oClass <- Async[F].delay(db.getClass(testSchema.name)).toResource

      // metadata assertions
      _ <- assertIO(oClass.getName.pure[F], testSchema.name, "Test Class was not created").toResource
      _ <- assertIO(oClass.getProperty(StringParamName).getName.pure[F], StringParamName).toResource
      _ <- assertIOBoolean(oClass.getProperty(StringParamName).isMandatory.pure[F]).toResource
      _ <- assertIOBoolean(oClass.getProperty(StringParamName).isReadonly.pure[F]).toResource
      _ <- assertIO(oClass.getProperty(StringParamName).isNotNull.pure[F], false).toResource

      orientGraph <- Sync[F].blocking(orientGraphFactory.getTx).toResource
      vertex <- Sync[F]
        .blocking(
          orientGraph.addVertex(s"class:${testSchema.name}", testSchema.encode(TestClass("test string value")).asJava)
        )
        .toResource

      // vertex assertions
      _ <- assertIO(vertex.getProperty[String](StringParamName).pure[F], "test string value").toResource

    } yield ()

    res.use_

  }

}
