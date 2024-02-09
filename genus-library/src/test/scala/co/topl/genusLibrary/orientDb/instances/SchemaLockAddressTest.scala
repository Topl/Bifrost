package co.topl.genusLibrary.orientDb.instances

import cats.implicits._
import co.topl.brambl.codecs.AddressCodecs
import co.topl.brambl.generators.{ModelGenerators => BramblGens}
import co.topl.brambl.models.LockAddress
import co.topl.genusLibrary.orientDb.instances.VertexSchemaInstances.instances.lockAddressSchema
import co.topl.genusLibrary.DbFixtureUtil
import co.topl.genusLibrary.orientDb.instances.SchemaLockAddress.Field
import co.topl.genusLibrary.orientDb.OrientThread
import co.topl.models.ModelGenerators.GenHelper
import com.orientechnologies.orient.core.metadata.schema.OType
import munit.{CatsEffectFunFixtures, CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import scala.jdk.CollectionConverters._

class SchemaLockAddressTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with CatsEffectFunFixtures
    with DbFixtureUtil {

  orientDbFixture.test("Address Schema Metadata") { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {
      databaseDocumentTx <- oThread.delay(odbFactory.getNoTx.getRawGraph).toResource

      oClass <- oThread.delay(databaseDocumentTx.getClass(Field.SchemaName)).toResource

      _ <- assertIO(oClass.getName.pure[F], Field.SchemaName, s"${Field.SchemaName} Class was not created").toResource

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

  orientDbFixture.test("Address Schema Add vertex Lock Address") {
    case (odbFactory, implicit0(oThread: OrientThread[F])) =>
      val res = for {
        dbTx <- oThread.delay(odbFactory.getTx).toResource

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
          .delay(dbTx.addVertex(s"class:${lockAddressSchema.name}", lockAddressSchema.encode(address).asJava))
          .toResource

        _ <- assertIO(
          vertex.getProperty[Int](lockAddressSchema.properties.filter(_.name == Field.Network).head.name).pure[F],
          address.network
        ).toResource

        _ <- assertIO(
          vertex.getProperty[Int](lockAddressSchema.properties.filter(_.name == Field.Ledger).head.name).pure[F],
          address.ledger
        ).toResource

        _ <- assertIO(
          vertex
            .getProperty[Array[Byte]](lockAddressSchema.properties.filter(_.name == Field.AddressId).head.name)
            .toSeq
            .pure[F],
          address.id.value.toByteArray.toSeq
        ).toResource

        _ <- assertIO(
          vertex
            .getProperty[String](lockAddressSchema.properties.filter(_.name == Field.AddressEncodedId).head.name)
            .pure[F],
          AddressCodecs.encodeAddress(address)
        ).toResource

        _ <- assertIO(
          oThread.delay(
            dbTx
              .getVertices(SchemaLockAddress.Field.AddressId, address.id.value.toByteArray)
              .iterator()
              .next()
              .getProperty[Array[Byte]](Field.AddressId)
              .toSeq
          ),
          address.id.value.toByteArray.toSeq
        ).toResource

      } yield ()
      res.use_

  }

}
