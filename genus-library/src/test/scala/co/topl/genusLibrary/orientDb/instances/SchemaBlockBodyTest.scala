package co.topl.genusLibrary.orientDb.instances

import cats.implicits._
import co.topl.genusLibrary.DbFixtureUtilV2
import co.topl.genusLibrary.orientDb.OrientThread
import co.topl.genusLibrary.orientDb.instances.SchemaBlockBody.Field
import co.topl.genusLibrary.orientDb.instances.VertexSchemaInstances.instances.blockBodySchema
import co.topl.node.models.BlockBody
import com.orientechnologies.orient.core.metadata.schema.OType
import munit.{CatsEffectFunFixtures, CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import scala.jdk.CollectionConverters._

class SchemaBlockBodyTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with CatsEffectFunFixtures
    with DbFixtureUtilV2 {

  orientDbFixtureV2.test("Block body Schema Metadata") { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {
      dbNoTx <- oThread.delay(odbFactory.getNoTx).toResource

      databaseDocumentTx <- oThread.delay(dbNoTx.getRawGraph).toResource

      oClass <- oThread.delay(databaseDocumentTx.getClass(Field.SchemaName)).toResource

      _ <- assertIO(oClass.getName.pure[F], Field.SchemaName, s"${Field.SchemaName} Class was not created").toResource

      txIdsProperty <- oClass.getProperty(Field.TransactionIds).pure[F].toResource
      _ <- (
        assertIO(txIdsProperty.getName.pure[F], Field.TransactionIds) &>
        assertIO(txIdsProperty.isMandatory.pure[F], false) &>
        assertIO(txIdsProperty.isReadonly.pure[F], false) &>
        assertIO(txIdsProperty.isNotNull.pure[F], false) &>
        assertIO(txIdsProperty.getType.pure[F], OType.BINARY)
      ).toResource

    } yield ()

    res.use_

  }

  orientDbFixtureV2.test("Block Body Schema Add vertex") { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {

      dbTx <- oThread.delay(odbFactory.getTx).toResource

      vertex <- oThread
        .delay(
          dbTx
            .addVertex(s"class:${blockBodySchema.name}", blockBodySchema.encode(BlockBody(Seq.empty)).asJava)
        )
        .toResource

      _ <- assertIO(
        vertex
          .getProperty[Array[Byte]](blockBodySchema.properties.filter(_.name == Field.TransactionIds).head.name)
          .toSeq
          .pure[F],
        Array.empty[Byte].toSeq
      ).toResource

    } yield ()
    res.use_

  }

}
