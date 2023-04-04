package co.topl.genusLibrary.orientDb.instances

import cats.effect.implicits.effectResourceOps
import cats.effect.kernel.Async
import cats.effect.{Resource, Sync}
import cats.implicits._
import co.topl.genusLibrary.orientDb.instances.SchemaAddress.Field
import co.topl.genusLibrary.orientDb.{DbFixtureUtil, OrientDBMetadataFactory}
import co.topl.models.ModelGenerators.GenHelper
import co.topl.brambl.generators.{ModelGenerators => BramblGens}
import co.topl.brambl.models.{Address, Identifier}
import com.orientechnologies.orient.core.metadata.schema.OType
import com.tinkerpop.blueprints.impls.orient.OrientGraphFactoryV2
import munit.{CatsEffectFunFixtures, CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import scala.jdk.CollectionConverters._
import scodec.bits.ByteVector

class SchemaAddressTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with CatsEffectFunFixtures
    with DbFixtureUtil {

  orientDbFixture.test("Address Schema Metadata") { odb =>
    val res = for {
      odbFactory <- Sync[F].blocking(new OrientGraphFactoryV2(odb, "testDb", "testUser", "testPass")).toResource
      dbNoTx     <- Sync[F].blocking(odbFactory.getNoTx).toResource
      _          <- Sync[F].blocking(dbNoTx.makeActive()).toResource

      databaseDocumentTx <- Resource.pure(odbFactory.getNoTx.getRawGraph)
      schema             <- SchemaAddress.make().pure[F].toResource
      _                  <- OrientDBMetadataFactory.createSchema[F](databaseDocumentTx, schema).toResource

      oClass <- Async[F].delay(databaseDocumentTx.getClass(schema.name)).toResource

      _ <- assertIO(oClass.getName.pure[F], schema.name, s"${schema.name} Class was not created").toResource

      networkProperty <- oClass.getProperty(Field.Network).pure[F].toResource
      _ <- (
        assertIO(networkProperty.getName.pure[F], Field.Network) &>
        assertIO(networkProperty.isMandatory.pure[F], true) &>
        assertIO(networkProperty.isReadonly.pure[F], true) &>
        assertIO(networkProperty.isNotNull.pure[F], true) &>
        assertIO(networkProperty.getType.pure[F], OType.INTEGER)
      ).toResource

      ledgerProperty <- oClass.getProperty(Field.Ledger).pure[F].toResource
      _ <- (
        assertIO(ledgerProperty.getName.pure[F], Field.Ledger) &>
        assertIO(ledgerProperty.isMandatory.pure[F], true) &>
        assertIO(ledgerProperty.isReadonly.pure[F], true) &>
        assertIO(ledgerProperty.isNotNull.pure[F], true) &>
        assertIO(ledgerProperty.getType.pure[F], OType.INTEGER)
      ).toResource

      indexProperty <- oClass.getProperty(Field.Index).pure[F].toResource
      _ <- (
        assertIO(indexProperty.getName.pure[F], Field.Index) &>
        assertIO(indexProperty.isMandatory.pure[F], false) &>
        assertIO(indexProperty.isReadonly.pure[F], true) &>
        assertIO(indexProperty.isNotNull.pure[F], false) &>
        assertIO(indexProperty.getType.pure[F], OType.INTEGER)
      ).toResource

      idProperty <- oClass.getProperty(Field.AddressId).pure[F].toResource
      _ <- (
        assertIO(idProperty.getName.pure[F], Field.AddressId) &>
        assertIO(idProperty.isMandatory.pure[F], true) &>
        assertIO(idProperty.isReadonly.pure[F], true) &>
        assertIO(idProperty.isNotNull.pure[F], true) &>
        assertIO(idProperty.getType.pure[F], OType.BINARY)
      ).toResource

    } yield ()

    res.use_

  }

  orientDbFixture.test("Address Schema Add vertex Lock Address") { odb =>
    val res = for {
      odbFactory <- Sync[F].blocking(new OrientGraphFactoryV2(odb, "testDb", "testUser", "testPass")).toResource

      dbNoTx <- Sync[F].blocking(odbFactory.getNoTx).toResource
      _      <- Sync[F].blocking(dbNoTx.makeActive()).toResource

      schema <- SchemaAddress.make().pure[F].toResource
      _      <- OrientDBMetadataFactory.createSchema[F](dbNoTx.getRawGraph, schema).toResource

      dbTx <- Sync[F].blocking(odbFactory.getTx).toResource
      _    <- Sync[F].blocking(dbTx.makeActive()).toResource

      address <- BramblGens.arbitraryLockAddress.arbitrary
        .map(lockAddress =>
          Address(
            lockAddress.network,
            lockAddress.ledger,
            0, // None when https://github.com/Topl/protobuf-specs/pull/49
            id = Identifier.of(Identifier.Value.Lock32(lockAddress.id.lock32.get))
          )
        )
        .first
        .pure[F]
        .toResource

      vertex <- Sync[F]
        .blocking(
          dbTx
            .addVertex(s"class:${schema.name}", schema.encode(address).asJava)
        )
        .toResource

      _ <- assertIO(
        vertex.getProperty[Int](schema.properties.filter(_.name == Field.Network).head.name).pure[F],
        address.network
      ).toResource

      _ <- assertIO(
        vertex.getProperty[Int](schema.properties.filter(_.name == Field.Ledger).head.name).pure[F],
        address.ledger
      ).toResource

      _ <- assertIO(
        vertex.getProperty[Int](schema.properties.filter(_.name == Field.Index).head.name).pure[F],
        address.index
      ).toResource

      _ <- assertIO(
        vertex.getProperty[Array[Byte]](schema.properties.filter(_.name == Field.AddressId).head.name).toSeq.pure[F],
        address.id.toByteArray.toSeq
      ).toResource

      _ <- logger.info(ByteVector(vertex.getProperty[Array[Byte]](Field.AddressId)).toBase64).toResource
      _ <- logger.info(ByteVector(address.id.toByteArray).toBase64).toResource

      _ <- assertIO(
        Sync[F].blocking(
          dbTx
            .getVertices(SchemaAddress.Field.AddressId, address.id.toByteArray)
            .iterator()
            .next()
            .getProperty[Array[Byte]](Field.AddressId)
            .toSeq
        ),
        address.id.toByteArray.toSeq
      ).toResource

    } yield ()
    res.use_

  }

  orientDbFixture.test("Address Schema Add vertex Transaction Output Address") { odb =>
    val res = for {
      odbFactory <- Sync[F].blocking(new OrientGraphFactoryV2(odb, "testDb", "testUser", "testPass")).toResource

      dbNoTx <- Sync[F].blocking(odbFactory.getNoTx).toResource
      _      <- Sync[F].blocking(dbNoTx.makeActive()).toResource

      schema <- SchemaAddress.make().pure[F].toResource
      _      <- OrientDBMetadataFactory.createSchema[F](dbNoTx.getRawGraph, schema).toResource

      dbTx <- Sync[F].blocking(odbFactory.getTx).toResource
      _    <- Sync[F].blocking(dbTx.makeActive()).toResource

      address <- BramblGens.arbitraryTransactionOutputAddress.arbitrary
        .map(outputAddress =>
          Address(
            outputAddress.network,
            outputAddress.ledger,
            outputAddress.index,
            id = Identifier.of(Identifier.Value.IoTransaction32(outputAddress.id.ioTransaction32.get))
          )
        )
        .first
        .pure[F]
        .toResource

      vertex <- Sync[F]
        .blocking(
          dbTx
            .addVertex(s"class:${schema.name}", schema.encode(address).asJava)
        )
        .toResource

      _ <- assertIO(
        vertex.getProperty[Int](schema.properties.filter(_.name == Field.Network).head.name).pure[F],
        address.network
      ).toResource

      _ <- assertIO(
        vertex.getProperty[Int](schema.properties.filter(_.name == Field.Ledger).head.name).pure[F],
        address.ledger
      ).toResource

      _ <- assertIO(
        vertex.getProperty[Int](schema.properties.filter(_.name == Field.Index).head.name).pure[F],
        address.index
      ).toResource

      _ <- assertIO(
        vertex.getProperty[Array[Byte]](schema.properties.filter(_.name == Field.AddressId).head.name).toSeq.pure[F],
        address.id.toByteArray.toSeq
      ).toResource

    } yield ()
    res.use_

  }

}
