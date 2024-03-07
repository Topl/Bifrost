package co.topl.genus.orientDb.instances

import cats.implicits._
import co.topl.brambl.generators.{ModelGenerators => BramblGenerator}
import co.topl.brambl.syntax._
import co.topl.genus.orientDb.OrientThread
import co.topl.genus.orientDb.instances.{SchemaBlockHeader, SchemaIoTransaction}
import co.topl.genus.DbFixtureUtil
import co.topl.genus.orientDb.instances.SchemaIoTransaction.Field
import co.topl.genus.orientDb.instances.VertexSchemaInstances.instances.{blockHeaderSchema, ioTransactionSchema}
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators
import com.orientechnologies.orient.core.metadata.schema.OType
import com.tinkerpop.blueprints.impls.orient.OrientVertex
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory

import scala.jdk.CollectionConverters._

class SchemaIoTransactionTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with DbFixtureUtil {

  orientDbFixture.test("IoTransaction Schema Metadata") { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {
      dbNoTx             <- oThread.delay(odbFactory.getNoTx).toResource
      databaseDocumentTx <- oThread.delay(dbNoTx.getRawGraph).toResource
      oClass             <- oThread.delay(databaseDocumentTx.getClass(Field.SchemaName)).toResource

      _ <- assertIO(oClass.getName.pure[F], Field.SchemaName, s"${Field.SchemaName} Class was not created").toResource

      transactionIdProperty <- oClass.getProperty(SchemaIoTransaction.Field.TransactionId).pure[F].toResource
      _ <- (
        assertIO(transactionIdProperty.getName.pure[F], SchemaIoTransaction.Field.TransactionId) &>
        assertIO(transactionIdProperty.isMandatory.pure[F], false) &>
        assertIO(transactionIdProperty.isReadonly.pure[F], true) &>
        assertIO(transactionIdProperty.isNotNull.pure[F], true) &>
        assertIO(transactionIdProperty.getType.pure[F], OType.BINARY)
      ).toResource

      transactionProperty <- oClass.getProperty(SchemaIoTransaction.Field.Transaction).pure[F].toResource
      _ <- (
        assertIO(transactionProperty.getName.pure[F], SchemaIoTransaction.Field.Transaction) &>
        assertIO(transactionProperty.isMandatory.pure[F], false) &>
        assertIO(transactionProperty.isReadonly.pure[F], false) &>
        assertIO(transactionProperty.isNotNull.pure[F], true) &>
        assertIO(transactionProperty.getType.pure[F], OType.BINARY)
      ).toResource

      sizeProperty <- oClass.getProperty(SchemaIoTransaction.Field.Size).pure[F].toResource
      _ <- (
        assertIO(sizeProperty.getName.pure[F], SchemaIoTransaction.Field.Size) &>
        assertIO(sizeProperty.isMandatory.pure[F], true) &>
        assertIO(sizeProperty.isReadonly.pure[F], true) &>
        assertIO(sizeProperty.isNotNull.pure[F], true) &>
        assertIO(sizeProperty.getType.pure[F], OType.LONG)
      ).toResource

      transactionLink <- oClass.getProperty(SchemaBlockHeader.Field.BlockId).pure[F].toResource
      _ <- (
        assertIO(transactionLink.getName.pure[F], SchemaBlockHeader.Field.BlockId) &>
        assertIO(transactionLink.getType.pure[F], OType.LINK) &>
        assertIO(transactionLink.getLinkedClass.getName.pure[F], SchemaBlockHeader.Field.SchemaName)
      ).toResource

    } yield ()

    res.use_

  }

  orientDbFixture.test("Transaction and Block Header Schema Add vertex") {
    case (odbFactory, implicit0(oThread: OrientThread[F])) =>
      val res = for {

        dbTx        <- oThread.delay(odbFactory.getTx).toResource
        blockHeader <- ModelGenerators.arbitraryHeader.arbitrary.first.pure[F].toResource
        transaction <- BramblGenerator.arbitraryIoTransaction.arbitrary.first.pure[F].toResource

        blockHeaderVertex <- oThread
          .delay(
            dbTx.addVertex(s"class:${blockHeaderSchema.name}", blockHeaderSchema.encode(blockHeader).asJava)
          )
          .toResource

        transactionVertex <- oThread.delay {
          val v = dbTx
            .addVertex(s"class:${ioTransactionSchema.name}", ioTransactionSchema.encode(transaction).asJava)
          v.setProperty(ioTransactionSchema.links.head.propertyName, blockHeaderVertex.getId)
          v
        }.toResource

        _ <- assertIO(
          transactionVertex
            .getProperty[Array[Byte]](
              ioTransactionSchema.properties.filter(_.name == SchemaIoTransaction.Field.TransactionId).head.name
            )
            .toSeq
            .pure[F],
          transaction.id.value.toByteArray.toSeq
        ).toResource

        _ <- assertIO(
          transactionVertex
            .getProperty[Long](
              ioTransactionSchema.properties.filter(_.name == SchemaIoTransaction.Field.Size).head.name
            )
            .pure[F],
          SchemaIoTransaction.size(transaction)
        ).toResource

        _ <- assertIO(
          oThread.delay(transactionVertex.getProperty[OrientVertex](SchemaBlockHeader.Field.BlockId)),
          blockHeaderVertex
        ).toResource

      } yield ()
      res.use_
  }

}
