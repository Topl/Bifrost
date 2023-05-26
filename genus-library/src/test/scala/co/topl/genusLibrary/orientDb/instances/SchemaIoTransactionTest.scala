package co.topl.genusLibrary.orientDb.instances

import cats.effect.implicits.effectResourceOps
import cats.effect.kernel.Async
import cats.effect.{IO, Resource}
import cats.implicits._
import co.topl.brambl.generators.{ModelGenerators => BramblGenerator}
import co.topl.brambl.syntax._
import co.topl.genusLibrary.orientDb.OrientDBMetadataFactory
import co.topl.genusLibrary.orientDb.OrientThread
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators
import com.orientechnologies.orient.core.metadata.schema.OType
import com.tinkerpop.blueprints.impls.orient.{OrientGraphFactory, OrientVertex}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.jdk.CollectionConverters._

class SchemaIoTransactionTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]
  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  test("IoTransaction Schema Metadata") {

    val res = for {
      implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
      orientGraphFactory                       <- Resource.pure(new OrientGraphFactory("memory:test"))
      db <- Resource.make(orientThread.delay(orientGraphFactory.getDatabase))(db =>
        orientThread.delay {
          db.drop()
          db.close()
        }
      )

      blockHeaderSchema = SchemaBlockHeader.make()
      _ <- OrientDBMetadataFactory.createVertex[F](db, blockHeaderSchema).toResource
      transactionSchema = SchemaIoTransaction.make()
      _ <- OrientDBMetadataFactory.createVertex[F](db, transactionSchema).toResource

      oClass <- Async[F].delay(db.getClass(transactionSchema.name)).toResource

      _ <- assertIO(
        oClass.getName.pure[F],
        transactionSchema.name,
        s"${transactionSchema.name} Class was not created"
      ).toResource

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

  test("Transaction and Block Header Schema Add vertex") {
    val res = for {
      implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
      orientGraphFactory                       <- Resource.pure(new OrientGraphFactory("memory:test"))
      db <- Resource.make(orientThread.delay(orientGraphFactory.getDatabase))(db =>
        orientThread.delay {
          db.drop()
          db.close()
        }
      )

      blockHeaderSchema = SchemaBlockHeader.make()
      _ <- OrientDBMetadataFactory.createVertex[F](db, blockHeaderSchema).toResource
      transactionSchema = SchemaIoTransaction.make()
      _ <- OrientDBMetadataFactory.createVertex[F](db, transactionSchema).toResource

      orientGraph <- orientThread.delay(orientGraphFactory.getTx).toResource
      _           <- orientThread.delay(orientGraph.makeActive()).toResource
      blockHeader <- ModelGenerators.arbitraryHeader.arbitrary.first.pure[F].toResource
      transaction <- BramblGenerator.arbitraryIoTransaction.arbitrary.first.pure[F].toResource

      blockHeaderVertex <- orientThread
        .delay(
          orientGraph
            .addVertex(s"class:${blockHeaderSchema.name}", blockHeaderSchema.encode(blockHeader).asJava)
        )
        .toResource

      transactionVertex <- orientThread.delay {
        val v = orientGraph
          .addVertex(s"class:${transactionSchema.name}", transactionSchema.encode(transaction).asJava)
        v.setProperty(transactionSchema.links.head.propertyName, blockHeaderVertex.getId)
        v
      }.toResource

      _ <- assertIO(
        transactionVertex
          .getProperty[Array[Byte]](
            transactionSchema.properties.filter(_.name == SchemaIoTransaction.Field.TransactionId).head.name
          )
          .toSeq
          .pure[F],
        transaction.id.value.toByteArray.toSeq
      ).toResource

      _ <- assertIO(
        transactionVertex
          .getProperty[Long](transactionSchema.properties.filter(_.name == SchemaIoTransaction.Field.Size).head.name)
          .pure[F],
        SchemaIoTransaction.size(transaction)
      ).toResource

      _ <- assertIO(
        orientThread.delay(transactionVertex.getProperty[OrientVertex](SchemaBlockHeader.Field.BlockId)),
        blockHeaderVertex
      ).toResource

    } yield ()
    res.use_
  }

}
