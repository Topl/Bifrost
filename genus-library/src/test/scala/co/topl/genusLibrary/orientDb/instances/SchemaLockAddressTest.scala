package co.topl.genusLibrary.orientDb.instances

import cats.effect.implicits.effectResourceOps
import cats.implicits._
import co.topl.genusLibrary.orientDb.instances.SchemaLockAddress.Field
import co.topl.genusLibrary.orientDb.{DbFixtureUtil, OrientDBMetadataFactory}
import co.topl.models.ModelGenerators.GenHelper
import co.topl.brambl.generators.{ModelGenerators => BramblGens}
import co.topl.brambl.models.LockAddress
import co.topl.brambl.utils.Encoding
import com.orientechnologies.orient.core.metadata.schema.OType
import com.tinkerpop.blueprints.impls.orient.OrientGraphFactoryV2
import munit.{CatsEffectFunFixtures, CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory

import scala.jdk.CollectionConverters._
import scodec.bits.ByteVector

class SchemaLockAddressTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with CatsEffectFunFixtures
    with DbFixtureUtil {

  orientDbFixture.test("Address Schema Metadata") { case (odb, oThread) =>
    val res = for {
      odbFactory <- oThread.delay(new OrientGraphFactoryV2(odb, "testDb", "testUser", "testPass")).toResource
      dbNoTx     <- oThread.delay(odbFactory.getNoTx).toResource
      _          <- oThread.delay(dbNoTx.makeActive()).toResource

      databaseDocumentTx <- oThread.delay(odbFactory.getNoTx.getRawGraph).toResource
      schema = SchemaLockAddress.make()
      _ <- OrientDBMetadataFactory.createVertex[F](databaseDocumentTx, schema).toResource

      oClass <- oThread.delay(databaseDocumentTx.getClass(schema.name)).toResource

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

      idProperty <- oClass.getProperty(Field.AddressId).pure[F].toResource
      _ <- (
        assertIO(idProperty.getName.pure[F], Field.AddressId) &>
        assertIO(idProperty.isMandatory.pure[F], true) &>
        assertIO(idProperty.isReadonly.pure[F], true) &>
        assertIO(idProperty.isNotNull.pure[F], true) &>
        assertIO(idProperty.getType.pure[F], OType.BINARY)
      ).toResource

      idProperty <- oClass.getProperty(Field.AddressEncodedId).pure[F].toResource
      _ <- (
        assertIO(idProperty.getName.pure[F], Field.AddressEncodedId) &>
        assertIO(idProperty.isMandatory.pure[F], true) &>
        assertIO(idProperty.isReadonly.pure[F], true) &>
        assertIO(idProperty.isNotNull.pure[F], true) &>
        assertIO(idProperty.getType.pure[F], OType.STRING)
      ).toResource

    } yield ()

    res.use_

  }

  orientDbFixture.test("Address Schema Add vertex Lock Address") { case (odb, oThread) =>
    val res = for {
      odbFactory <- oThread.delay(new OrientGraphFactoryV2(odb, "testDb", "testUser", "testPass")).toResource

      dbNoTx <- oThread.delay(odbFactory.getNoTx).toResource
      _      <- oThread.delay(dbNoTx.makeActive()).toResource

      schema = SchemaLockAddress.make()
      _ <- OrientDBMetadataFactory.createVertex[F](dbNoTx.getRawGraph, schema).toResource

      dbTx <- oThread.delay(odbFactory.getTx).toResource
      _    <- oThread.delay(dbTx.makeActive()).toResource

      address <- BramblGens.arbitraryLockAddress.arbitrary
        .map(lockAddress =>
          LockAddress(
            lockAddress.network,
            lockAddress.ledger,
            lockAddress.id
          )
        )
        .first
        .pure[F]
        .toResource

      vertex <- oThread
        .delay(
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
        vertex.getProperty[Array[Byte]](schema.properties.filter(_.name == Field.AddressId).head.name).toSeq.pure[F],
        address.id.toByteArray.toSeq
      ).toResource

      _ <- assertIO(
        vertex.getProperty[String](schema.properties.filter(_.name == Field.AddressEncodedId).head.name).pure[F],
        Encoding.encodeToBase58(address.id.toByteArray)
      ).toResource

      _ <- assertIO(
        oThread.delay(
          dbTx
            .getVertices(SchemaLockAddress.Field.AddressId, address.id.toByteArray)
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

}
