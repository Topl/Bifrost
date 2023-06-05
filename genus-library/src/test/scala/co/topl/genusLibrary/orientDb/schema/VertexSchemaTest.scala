package co.topl.genusLibrary.orientDb.schema

import cats.implicits._
import co.topl.genusLibrary.DbFixtureUtil
import co.topl.genusLibrary.orientDb.OrientDBMetadataFactory
import co.topl.genusLibrary.orientDb.schema.OTyped.Instances._
import com.tinkerpop.blueprints.impls.orient.OrientGraphFactoryV2
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import scala.jdk.CollectionConverters._

class VertexSchemaTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory with DbFixtureUtil {

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

  orientDbFixture.test("Test Schema Metadata") { case (odb, oThread) =>
    val res = for {
      odbFactory         <- oThread.delay(new OrientGraphFactoryV2(odb, "testDb", "testUser", "testPass")).toResource
      databaseDocumentTx <- oThread.delay(odbFactory.getNoTx.getRawGraph).toResource
      _                  <- OrientDBMetadataFactory.createVertex[F](databaseDocumentTx, testSchema).toResource

      oClass <- oThread.delay(databaseDocumentTx.getClass(testSchema.name)).toResource

      _ <- assertIO(oClass.getName.pure[F], testSchema.name, "Test Class was not created").toResource
      _ <- assertIO(oClass.getProperty(StringParamName).getName.pure[F], StringParamName).toResource
      _ <- assertIOBoolean(oClass.getProperty(StringParamName).isMandatory.pure[F]).toResource
      _ <- assertIOBoolean(oClass.getProperty(StringParamName).isReadonly.pure[F]).toResource
      _ <- assertIO(oClass.getProperty(StringParamName).isNotNull.pure[F], false).toResource

    } yield ()

    res.use_

  }

  orientDbFixture.test("Test Schema Add Vertex") { case (odb, oThread) =>
    val res = for {
      odbFactory         <- oThread.delay(new OrientGraphFactoryV2(odb, "testDb", "testUser", "testPass")).toResource
      databaseDocumentTx <- oThread.delay(odbFactory.getNoTx.getRawGraph).toResource
      _                  <- OrientDBMetadataFactory.createVertex[F](databaseDocumentTx, testSchema).toResource

      dbTx <- oThread.delay(odbFactory.getTx).toResource
      _    <- oThread.delay(dbTx.makeActive()).toResource

      vertex <- oThread
        .delay(
          dbTx.addVertex(s"class:${testSchema.name}", testSchema.encode(TestClass("test string value")).asJava)
        )
        .toResource

      _ <- assertIO(vertex.getProperty[String](StringParamName).pure[F], "test string value").toResource

    } yield ()

    res.use_

  }

}
