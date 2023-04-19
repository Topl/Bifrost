package co.topl.genusLibrary.orientDb.instances

import cats.effect.implicits.effectResourceOps
import cats.effect.kernel.Async
import cats.effect.{Resource, Sync}
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

  orientDbFixture.test("Canonical Head Schema Metadata") { odb =>
    val res = for {
      odbFactory <- Sync[F].blocking(new OrientGraphFactoryV2(odb, "testDb", "testUser", "testPass")).toResource
      dbNoTx     <- Sync[F].blocking(odbFactory.getNoTx).toResource
      _          <- Sync[F].blocking(dbNoTx.makeActive()).toResource

      databaseDocumentTx <- Resource.pure(odbFactory.getNoTx.getRawGraph)

      blockHeaderSchema   <- SchemaBlockHeader.make().pure[F].toResource
      _                   <- OrientDBMetadataFactory.createSchema[F](databaseDocumentTx, blockHeaderSchema).toResource
      canonicalHeadSchema <- SchemaCanonicalHead.make().pure[F].toResource
      _                   <- OrientDBMetadataFactory.createSchema[F](databaseDocumentTx, canonicalHeadSchema).toResource

      oClass <- Async[F].delay(databaseDocumentTx.getClass(canonicalHeadSchema.name)).toResource

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

  orientDbFixture.test("Canonical Header Schema Add vertex") { odb =>
    val res = for {
      odbFactory <- Sync[F].blocking(new OrientGraphFactoryV2(odb, "testDb", "testUser", "testPass")).toResource

      dbNoTx <- Sync[F].blocking(odbFactory.getNoTx).toResource
      _      <- Sync[F].blocking(dbNoTx.makeActive()).toResource

      blockHeaderSchema <- SchemaBlockHeader.make().pure[F].toResource
      _                 <- OrientDBMetadataFactory.createSchema[F](dbNoTx.getRawGraph, blockHeaderSchema).toResource

      canonicalHeadSchema <- SchemaCanonicalHead.make().pure[F].toResource
      _                   <- OrientDBMetadataFactory.createSchema[F](dbNoTx.getRawGraph, canonicalHeadSchema).toResource

      dbTx <- Sync[F].blocking(odbFactory.getTx).toResource
      _    <- Sync[F].blocking(dbTx.makeActive()).toResource

      blockHeader <- ModelGenerators.arbitraryHeader.arbitrary.first.pure[F].toResource

      blockHeaderVertex <- Sync[F]
        .blocking(
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

      canonicalHeadVertex <- Sync[F].blocking {
        val v = dbTx
          .addVertex(s"class:${canonicalHeadSchema.name}", canonicalHeadSchema.encode(CanonicalHead).asJava)
        v.setProperty(canonicalHeadSchema.links.head.propertyName, blockHeaderVertex.getId)
        v
      }.toResource

      blockHeaderFromCanonicalHead <- Sync[F].blocking {
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
