package co.topl.genusLibrary.orientDb.instances

import cats.effect.implicits.effectResourceOps
import cats.effect.kernel.Async
import cats.effect.{IO, Resource, Sync}
import cats.implicits._
import co.topl.genusLibrary.orientDb.OrientDBMetadataFactory
import co.topl.genusLibrary.orientDb.instances.SchemaBlockHeader.Field
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators
import com.orientechnologies.orient.core.metadata.schema.OType
import com.tinkerpop.blueprints.impls.orient.OrientGraphFactory
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.jdk.CollectionConverters._

class SchemaBlockHeaderSuite extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]
  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  test("Test Block Header Schema Metadata") {

    val g: OrientGraphFactory = new OrientGraphFactory("memory:test")

    val res = for {
      orientGraphFactory <- Resource.make[F, OrientGraphFactory](Sync[F].blocking(g))(g => Sync[F].delay(g.close()))
      db     <- Resource.make(Sync[F].blocking(orientGraphFactory.getDatabase))(db => Sync[F].delay(db.close()))
      schema <- SchemaBlockHeader.make().pure[F].toResource
      _      <- OrientDBMetadataFactory.createVertex[F](db, schema)

      oClass <- Async[F].delay(db.getClass(schema.name)).toResource

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

  test("Test Block Header Schema Add Vertex") {
    val g: OrientGraphFactory = new OrientGraphFactory("memory:test")

    val res = for {

      orientGraphFactory <- Resource.make[F, OrientGraphFactory](Sync[F].blocking(g))(g => Sync[F].delay(g.close()))
      db          <- Resource.make(Sync[F].blocking(orientGraphFactory.getDatabase))(db => Sync[F].delay(db.close()))
      schema      <- SchemaBlockHeader.make().pure[F].toResource
      _           <- OrientDBMetadataFactory.createVertex[F](db, schema)
      orientGraph <- Sync[F].blocking(orientGraphFactory.getTx).toResource
      blockHeader <- ModelGenerators.arbitraryHeader.arbitrary.first.pure[F].toResource
      vertex <- Sync[F]
        .blocking(
          orientGraph
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
