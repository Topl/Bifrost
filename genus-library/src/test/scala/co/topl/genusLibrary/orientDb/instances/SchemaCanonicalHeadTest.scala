package co.topl.genusLibrary.orientDb.instances

import cats.effect.implicits.effectResourceOps
import cats.implicits._
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.genusLibrary.orientDb.instances.SchemaBlockHeader.Field
import co.topl.genusLibrary.orientDb.instances.SchemaCanonicalHead.CanonicalHead
import co.topl.genusLibrary.orientDb.{DbFixtureUtil, OrientDBMetadataFactory}
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators
import com.orientechnologies.orient.core.metadata.schema.OType
import com.tinkerpop.blueprints.impls.orient.{OrientGraphFactoryV2, OrientVertex}
import munit.{CatsEffectFunFixtures, CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory

import scala.jdk.CollectionConverters._

class SchemaCanonicalHeadTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with CatsEffectFunFixtures
    with DbFixtureUtil {

  orientDbFixture.test("Canonical Head Schema Metadata") { case (odb, oThread) =>
    val res = for {
      odbFactory <- oThread.delay(new OrientGraphFactoryV2(odb, "testDb", "testUser", "testPass")).toResource
      dbNoTx     <- oThread.delay(odbFactory.getNoTx).toResource
      _          <- oThread.delay(dbNoTx.makeActive()).toResource

      databaseDocumentTx <- oThread.delay(odbFactory.getNoTx.getRawGraph).toResource
      _                  <- oThread.delay(databaseDocumentTx.activateOnCurrentThread()).toResource

      blockHeaderSchema = SchemaBlockHeader.make()
      _ <- OrientDBMetadataFactory.createVertex[F](databaseDocumentTx, blockHeaderSchema).toResource
      canonicalHeadSchema = SchemaCanonicalHead.make()
      _ <- OrientDBMetadataFactory.createVertex[F](databaseDocumentTx, canonicalHeadSchema).toResource

      oClass <- oThread.delay(databaseDocumentTx.getClass(canonicalHeadSchema.name)).toResource

      _ <- assertIO(
        oClass.getName.pure[F],
        canonicalHeadSchema.name,
        s"${canonicalHeadSchema.name} Class was not created"
      ).toResource

      canonicalHeadLink <- oClass.getProperty(SchemaBlockHeader.Field.BlockId).pure[F].toResource
      _ <- (
        assertIO(canonicalHeadLink.getName.pure[F], SchemaBlockHeader.Field.BlockId) &>
        assertIO(canonicalHeadLink.getType.pure[F], OType.LINK) &>
        assertIO(canonicalHeadLink.getLinkedClass.getName.pure[F], SchemaBlockHeader.Field.SchemaName)
      ).toResource

    } yield ()

    res.use_

  }

  orientDbFixture.test("Canonical Header Schema Add vertex") { case (odb, oThread) =>
    val res = for {
      odbFactory <- oThread.delay(new OrientGraphFactoryV2(odb, "testDb", "testUser", "testPass")).toResource

      dbNoTx <- oThread.delay(odbFactory.getNoTx).toResource
      _      <- oThread.delay(dbNoTx.makeActive()).toResource

      blockHeaderSchema = SchemaBlockHeader.make()
      rawGraph <- oThread.delay(dbNoTx.getRawGraph).toResource
      _        <- oThread.delay(rawGraph.activateOnCurrentThread()).toResource
      _        <- OrientDBMetadataFactory.createVertex[F](rawGraph, blockHeaderSchema).toResource

      canonicalHeadSchema = SchemaCanonicalHead.make()
      _ <- OrientDBMetadataFactory.createVertex[F](rawGraph, canonicalHeadSchema).toResource

      dbTx <- oThread.delay(odbFactory.getTx).toResource
      _    <- oThread.delay(dbTx.makeActive()).toResource

      blockHeader <- ModelGenerators.arbitraryHeader.arbitrary.first.embedId.pure[F].toResource

      blockHeaderVertex <- oThread
        .delay(
          dbTx
            .addVertex(s"class:${blockHeaderSchema.name}", blockHeaderSchema.encode(blockHeader).asJava)
        )
        .toResource

      _ <- assertIO(
        blockHeaderVertex
          .getProperty[Array[Byte]](blockHeaderSchema.properties.filter(_.name == Field.BlockId).head.name)
          .toSeq
          .pure[F],
        blockHeader.id.value.toByteArray.toSeq
      ).toResource

      canonicalHeadVertex <- oThread.delay {
        val v = dbTx
          .addVertex(s"class:${canonicalHeadSchema.name}", canonicalHeadSchema.encode(CanonicalHead).asJava)
        v.setProperty(canonicalHeadSchema.links.head.propertyName, blockHeaderVertex.getId)
        v
      }.toResource

      blockHeaderFromCanonicalHead <- oThread.delay {
        canonicalHeadVertex.getProperty[OrientVertex](SchemaBlockHeader.Field.BlockId)
      }.toResource

      _ <- assertIO(
        blockHeaderFromCanonicalHead.pure[F],
        blockHeaderVertex
      ).toResource

      blockHeaderDecoded = blockHeaderSchema.decodeVertex(blockHeaderFromCanonicalHead)
      _ <- assertIOBoolean((blockHeader == blockHeaderDecoded).pure[F]).toResource

    } yield ()
    res.use_

  }

}
