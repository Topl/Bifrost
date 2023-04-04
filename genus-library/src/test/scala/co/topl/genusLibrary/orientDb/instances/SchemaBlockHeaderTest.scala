package co.topl.genusLibrary.orientDb.instances

import cats.effect.implicits.effectResourceOps
import cats.effect.kernel.Async
import cats.effect.{Resource, Sync}
import cats.implicits._
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.genusLibrary.orientDb.{DbFixtureUtil, OrientDBMetadataFactory}
import co.topl.genusLibrary.orientDb.instances.SchemaBlockHeader.Field
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators
import com.orientechnologies.orient.core.metadata.schema.OType
import com.tinkerpop.blueprints.impls.orient.OrientGraphFactoryV2
import munit.{CatsEffectFunFixtures, CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import scala.jdk.CollectionConverters._

class SchemaBlockHeaderTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with CatsEffectFunFixtures
    with DbFixtureUtil {

  orientDbFixture.test("Block Header Schema Metadata") { odb =>
    val res = for {
      odbFactory <- Sync[F].blocking(new OrientGraphFactoryV2(odb, "testDb", "testUser", "testPass")).toResource
      dbNoTx     <- Sync[F].blocking(odbFactory.getNoTx).toResource
      _          <- Sync[F].blocking(dbNoTx.makeActive()).toResource

      databaseDocumentTx <- Resource.pure(odbFactory.getNoTx.getRawGraph)
      schema             <- SchemaBlockHeader.make().pure[F].toResource
      _                  <- OrientDBMetadataFactory.createSchema[F](databaseDocumentTx, schema).toResource

      oClass <- Async[F].delay(databaseDocumentTx.getClass(schema.name)).toResource

      _ <- assertIO(oClass.getName.pure[F], schema.name, s"${schema.name} Class was not created").toResource

      blockIdProperty <- oClass.getProperty(Field.BlockId).pure[F].toResource
      _ <- (
        assertIO(blockIdProperty.getName.pure[F], Field.BlockId) &>
        assertIOBoolean(blockIdProperty.isMandatory.pure[F]) &>
        assertIOBoolean(blockIdProperty.isReadonly.pure[F]) &>
        assertIOBoolean(blockIdProperty.isNotNull.pure[F]) &>
        assertIO(blockIdProperty.getType.pure[F], OType.BINARY)
      ).toResource

      parentHeaderIdProperty <- oClass.getProperty(Field.ParentHeaderId).pure[F].toResource
      _ <- (
        assertIO(parentHeaderIdProperty.getName.pure[F], Field.ParentHeaderId) &>
        assertIOBoolean(parentHeaderIdProperty.isMandatory.pure[F]) &>
        assertIOBoolean(parentHeaderIdProperty.isReadonly.pure[F]) &>
        assertIOBoolean(parentHeaderIdProperty.isNotNull.pure[F]) &>
        assertIO(parentHeaderIdProperty.getType.pure[F], OType.BINARY)
      ).toResource

      parentSlotProperty <- oClass.getProperty(Field.ParentSlot).pure[F].toResource
      _ <- (
        assertIO(parentSlotProperty.getName.pure[F], Field.ParentSlot) &>
        assertIO(parentSlotProperty.isMandatory.pure[F], false) &>
        assertIO(parentSlotProperty.isReadonly.pure[F], true) &>
        assertIO(parentSlotProperty.isNotNull.pure[F], false) &>
        assertIO(parentSlotProperty.getType.pure[F], OType.LONG)
      ).toResource

      txRootProperty <- oClass.getProperty(Field.TxRoot).pure[F].toResource
      _ <- (
        assertIO(txRootProperty.getName.pure[F], Field.TxRoot) &>
        assertIO(txRootProperty.isMandatory.pure[F], false) &>
        assertIO(txRootProperty.isReadonly.pure[F], true) &>
        assertIO(txRootProperty.isNotNull.pure[F], false) &>
        assertIO(txRootProperty.getType.pure[F], OType.BINARY)
      ).toResource

      bloomFilterProperty <- oClass.getProperty(Field.BloomFilter).pure[F].toResource
      _ <- (
        assertIO(bloomFilterProperty.getName.pure[F], Field.BloomFilter) &>
        assertIO(bloomFilterProperty.isMandatory.pure[F], false) &>
        assertIO(bloomFilterProperty.isReadonly.pure[F], true) &>
        assertIO(bloomFilterProperty.isNotNull.pure[F], false) &>
        assertIO(bloomFilterProperty.getType.pure[F], OType.BINARY)
      ).toResource

      timestampProperty <- oClass.getProperty(Field.Timestamp).pure[F].toResource
      _ <- (
        assertIO(timestampProperty.getName.pure[F], Field.Timestamp) &>
        assertIO(timestampProperty.isMandatory.pure[F], true) &>
        assertIO(timestampProperty.isReadonly.pure[F], true) &>
        assertIO(timestampProperty.isNotNull.pure[F], true) &>
        assertIO(timestampProperty.getType.pure[F], OType.LONG)
      ).toResource

      heightProperty <- oClass.getProperty(Field.Height).pure[F].toResource
      _ <- (
        assertIO(heightProperty.getName.pure[F], Field.Height) &>
        assertIO(heightProperty.isMandatory.pure[F], true) &>
        assertIO(heightProperty.isReadonly.pure[F], true) &>
        assertIO(heightProperty.isNotNull.pure[F], true) &>
        assertIO(heightProperty.getType.pure[F], OType.LONG)
      ).toResource

      slotProperty <- oClass.getProperty(Field.Slot).pure[F].toResource
      _ <- (
        assertIO(slotProperty.getName.pure[F], Field.Slot) &>
        assertIO(slotProperty.isMandatory.pure[F], true) &>
        assertIO(slotProperty.isReadonly.pure[F], true) &>
        assertIO(slotProperty.isNotNull.pure[F], true) &>
        assertIO(slotProperty.getType.pure[F], OType.LONG)
      ).toResource

      eligibilityCertificateProperty <- oClass.getProperty(Field.EligibilityCertificate).pure[F].toResource
      _ <- (
        assertIO(eligibilityCertificateProperty.getName.pure[F], Field.EligibilityCertificate) &>
        assertIO(eligibilityCertificateProperty.isMandatory.pure[F], true) &>
        assertIO(eligibilityCertificateProperty.isReadonly.pure[F], true) &>
        assertIO(eligibilityCertificateProperty.isNotNull.pure[F], true) &>
        assertIO(eligibilityCertificateProperty.getType.pure[F], OType.BINARY)
      ).toResource

      operationalCertificateProperty <- oClass.getProperty(Field.OperationalCertificate).pure[F].toResource
      _ <- (
        assertIO(operationalCertificateProperty.getName.pure[F], Field.OperationalCertificate) &>
        assertIO(operationalCertificateProperty.isMandatory.pure[F], true) &>
        assertIO(operationalCertificateProperty.isReadonly.pure[F], true) &>
        assertIO(operationalCertificateProperty.isNotNull.pure[F], true) &>
        assertIO(operationalCertificateProperty.getType.pure[F], OType.BINARY)
      ).toResource

      metadataProperty <- oClass.getProperty(Field.Metadata).pure[F].toResource
      _ <- (
        assertIO(metadataProperty.getName.pure[F], Field.Metadata) &>
        assertIO(metadataProperty.isMandatory.pure[F], true) &>
        assertIO(metadataProperty.isReadonly.pure[F], true) &>
        assertIO(metadataProperty.isNotNull.pure[F], false) &>
        assertIO(metadataProperty.getType.pure[F], OType.BINARY)
      ).toResource

      addressProperty <- oClass.getProperty(Field.Address).pure[F].toResource
      _ <- (
        assertIO(addressProperty.getName.pure[F], Field.Address) &>
        assertIO(addressProperty.isMandatory.pure[F], true) &>
        assertIO(addressProperty.isReadonly.pure[F], true) &>
        assertIO(addressProperty.isNotNull.pure[F], true) &>
        assertIO(addressProperty.getType.pure[F], OType.BINARY)
      ).toResource

    } yield ()

    res.use_

  }

  orientDbFixture.test("Block Header Schema Add vertex") { odb =>
    val res = for {
      odbFactory <- Sync[F].blocking(new OrientGraphFactoryV2(odb, "testDb", "testUser", "testPass")).toResource

      dbNoTx <- Sync[F].blocking(odbFactory.getNoTx).toResource
      _      <- Sync[F].blocking(dbNoTx.makeActive()).toResource

      schema <- SchemaBlockHeader.make().pure[F].toResource
      _      <- OrientDBMetadataFactory.createSchema[F](dbNoTx.getRawGraph, schema).toResource

      dbTx <- Sync[F].blocking(odbFactory.getTx).toResource
      _    <- Sync[F].blocking(dbTx.makeActive()).toResource

      blockHeader <- ModelGenerators.arbitraryHeader.arbitrary.first.pure[F].toResource
      vertex <- Sync[F]
        .blocking(
          dbTx
            .addVertex(s"class:${schema.name}", schema.encode(blockHeader).asJava)
        )
        .toResource

      _ <- assertIO(
        vertex.getProperty[Array[Byte]](schema.properties.filter(_.name == Field.BlockId).head.name).toSeq.pure[F],
        blockHeader.id.value.toByteArray.toSeq
      ).toResource

      _ <- assertIO(
        vertex
          .getProperty[Array[Byte]](schema.properties.filter(_.name == Field.ParentHeaderId).head.name)
          .toSeq
          .pure[F],
        blockHeader.parentHeaderId.value.toByteArray.toSeq
      ).toResource

      _ <- assertIO(
        vertex.getProperty[Long](schema.properties.filter(_.name == Field.ParentSlot).head.name).pure[F],
        blockHeader.parentSlot
      ).toResource

      _ <- assertIO(
        vertex.getProperty[Array[Byte]](schema.properties.filter(_.name == Field.TxRoot).head.name).toSeq.pure[F],
        blockHeader.txRoot.toByteArray.toSeq
      ).toResource

      _ <- assertIO(
        vertex.getProperty[Array[Byte]](schema.properties.filter(_.name == Field.BloomFilter).head.name).toSeq.pure[F],
        blockHeader.bloomFilter.toByteArray.toSeq
      ).toResource

      _ <- assertIO(
        vertex.getProperty[Long](schema.properties.filter(_.name == Field.Timestamp).head.name).pure[F],
        blockHeader.timestamp
      ).toResource

      _ <- assertIO(
        vertex.getProperty[Long](schema.properties.filter(_.name == Field.Height).head.name).pure[F],
        blockHeader.height
      ).toResource

      _ <- assertIO(
        vertex.getProperty[Long](schema.properties.filter(_.name == Field.Slot).head.name).pure[F],
        blockHeader.slot
      ).toResource

      _ <- assertIO(
        vertex
          .getProperty[Array[Byte]](schema.properties.filter(_.name == Field.EligibilityCertificate).head.name)
          .toSeq
          .pure[F],
        blockHeader.eligibilityCertificate.toByteArray.toSeq
      ).toResource

      _ <- assertIO(
        vertex
          .getProperty[Array[Byte]](schema.properties.filter(_.name == Field.OperationalCertificate).head.name)
          .toSeq
          .pure[F],
        blockHeader.operationalCertificate.toByteArray.toSeq
      ).toResource

      _ <- assertIO(
        vertex.getProperty[Array[Byte]](schema.properties.filter(_.name == Field.Metadata).head.name).toSeq.pure[F],
        blockHeader.metadata.toByteArray.toSeq
      ).toResource

      _ <- assertIO(
        vertex.getProperty[Array[Byte]](schema.properties.filter(_.name == Field.Address).head.name).toSeq.pure[F],
        blockHeader.address.toByteArray.toSeq
      ).toResource

    } yield ()
    res.use_

  }

}
