package co.topl.genusLibrary.orientDb.instances

import cats.effect.implicits.effectResourceOps
import cats.implicits._
import co.topl.brambl.generators.{ModelGenerators => BramblGens}
import co.topl.genus.services.{Txo, TxoState}
import co.topl.genusLibrary.orientDb.instances.SchemaTxo.Field
import co.topl.genusLibrary.orientDb.{DbFixtureUtil, OrientDBMetadataFactory}
import co.topl.models.ModelGenerators.GenHelper
import com.orientechnologies.orient.core.metadata.schema.OType
import com.tinkerpop.blueprints.impls.orient.OrientGraphFactoryV2
import munit.{CatsEffectFunFixtures, CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import scala.jdk.CollectionConverters._

class SchemaTxoTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with CatsEffectFunFixtures
    with DbFixtureUtil {

  orientDbFixture.test("Txo Schema Metadata") { case (odb, oThread) =>
    val res = for {
      odbFactory <- oThread.delay(new OrientGraphFactoryV2(odb, "testDb", "testUser", "testPass")).toResource
      dbNoTx     <- oThread.delay(odbFactory.getNoTx).toResource
      _          <- oThread.delay(dbNoTx.makeActive()).toResource

      databaseDocumentTx <- oThread.delay(odbFactory.getNoTx.getRawGraph).toResource
      schema = SchemaTxo.make()
      _ <- OrientDBMetadataFactory.createVertex[F](databaseDocumentTx, schema).toResource

      oClass <- oThread.delay(databaseDocumentTx.getClass(schema.name)).toResource

      _ <- assertIO(oClass.getName.pure[F], schema.name, s"${schema.name} Class was not created").toResource

      networkProperty <- oClass.getProperty(Field.TransactionOutput).pure[F].toResource
      _ <- (
        assertIO(networkProperty.getName.pure[F], Field.TransactionOutput) &>
        assertIO(networkProperty.isMandatory.pure[F], true) &>
        assertIO(networkProperty.isReadonly.pure[F], true) &>
        assertIO(networkProperty.isNotNull.pure[F], true) &>
        assertIO(networkProperty.getType.pure[F], OType.BINARY)
      ).toResource

      ledgerProperty <- oClass.getProperty(Field.State).pure[F].toResource
      _ <- (
        assertIO(ledgerProperty.getName.pure[F], Field.State) &>
        assertIO(ledgerProperty.isMandatory.pure[F], true) &>
        assertIO(ledgerProperty.isReadonly.pure[F], true) &>
        assertIO(ledgerProperty.isNotNull.pure[F], true) &>
        assertIO(ledgerProperty.getType.pure[F], OType.INTEGER)
      ).toResource

      idProperty <- oClass.getProperty(Field.OutputAddress).pure[F].toResource
      _ <- (
        assertIO(idProperty.getName.pure[F], Field.OutputAddress) &>
        assertIO(idProperty.isMandatory.pure[F], true) &>
        assertIO(idProperty.isReadonly.pure[F], true) &>
        assertIO(idProperty.isNotNull.pure[F], true) &>
        assertIO(idProperty.getType.pure[F], OType.BINARY)
      ).toResource

    } yield ()

    res.use_

  }

  orientDbFixture.test("Txo Schema Add vertex") { case (odb, oThread) =>
    val res = for {
      odbFactory <- oThread.delay(new OrientGraphFactoryV2(odb, "testDb", "testUser", "testPass")).toResource

      dbNoTx <- oThread.delay(odbFactory.getNoTx).toResource
      _      <- oThread.delay(dbNoTx.makeActive()).toResource

      schema = SchemaTxo.make()
      _ <- OrientDBMetadataFactory.createVertex[F](dbNoTx.getRawGraph, schema).toResource

      dbTx <- oThread.delay(odbFactory.getTx).toResource
      _    <- oThread.delay(dbTx.makeActive()).toResource

      outputAddress <- BramblGens.arbitraryTransactionOutputAddress.arbitrary.first
        .pure[F]
        .toResource

      transactionOutput <- BramblGens.arbitraryUnspentTransactionOutput.arbitrary.first
        .pure[F]
        .toResource

      txo = Txo(transactionOutput, TxoState.UNSPENT, outputAddress)

      vertex <- oThread
        .delay(
          dbTx
            .addVertex(s"class:${schema.name}", schema.encode(txo).asJava)
        )
        .toResource

      _ <- assertIO(
        vertex
          .getProperty[Array[Byte]](schema.properties.filter(_.name == Field.TransactionOutput).head.name)
          .toSeq
          .pure[F],
        txo.transactionOutput.toByteArray.toSeq
      ).toResource

      _ <- assertIO(
        vertex.getProperty[Int](schema.properties.filter(_.name == Field.State).head.name).pure[F],
        txo.state.value
      ).toResource

      _ <- assertIO(
        vertex
          .getProperty[Array[Byte]](schema.properties.filter(_.name == Field.OutputAddress).head.name)
          .toSeq
          .pure[F],
        txo.outputAddress.toByteArray.toSeq
      ).toResource

    } yield ()
    res.use_

  }

}
