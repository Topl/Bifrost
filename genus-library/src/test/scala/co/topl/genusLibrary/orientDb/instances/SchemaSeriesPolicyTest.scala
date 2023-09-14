package co.topl.genusLibrary.orientDb.instances

import cats.implicits._
import co.topl.brambl.generators.{ModelGenerators => BramblGenerator}
import co.topl.brambl.models.Event.SeriesPolicy
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.syntax.seriesPolicyAsSeriesPolicySyntaxOps
import co.topl.genusLibrary.DbFixtureUtil
import co.topl.genusLibrary.orientDb.OrientThread
import co.topl.genusLibrary.orientDb.instances.SchemaSeriesPolicy.Field
import co.topl.genusLibrary.orientDb.instances.VertexSchemaInstances.instances.seriesPolicySchema
import co.topl.models.ModelGenerators.GenHelper
import com.orientechnologies.orient.core.metadata.schema.OType
import munit.{CatsEffectFunFixtures, CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import scala.jdk.CollectionConverters._

class SchemaSeriesPolicyTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with CatsEffectFunFixtures
    with DbFixtureUtil {

  orientDbFixture.test("SeriesPolicy Schema Metadata") { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {
      dbNoTx             <- oThread.delay(odbFactory.getNoTx).toResource
      databaseDocumentTx <- oThread.delay(dbNoTx.getRawGraph).toResource
      oClass             <- oThread.delay(databaseDocumentTx.getClass(Field.SchemaName)).toResource

      _ <- assertIO(oClass.getName.pure[F], Field.SchemaName, s"${Field.SchemaName} Class was not created").toResource

      p <- oClass.getProperty(Field.Label).pure[F].toResource
      _ <- (
        assertIO(p.getName.pure[F], Field.Label) &>
        assertIO(p.isMandatory.pure[F], true) &>
        assertIO(p.isReadonly.pure[F], true) &>
        assertIO(p.isNotNull.pure[F], true) &>
        assertIO(p.getType.pure[F], OType.STRING)
      ).toResource

      p <- oClass.getProperty(Field.RegistrationUtxo).pure[F].toResource
      _ <- (
        assertIO(p.getName.pure[F], Field.RegistrationUtxo) &>
        assertIO(p.isMandatory.pure[F], true) &>
        assertIO(p.isReadonly.pure[F], true) &>
        assertIO(p.isNotNull.pure[F], true) &>
        assertIO(p.getType.pure[F], OType.BINARY)
      ).toResource

      p <- oClass.getProperty(Field.QuantityDescriptor).pure[F].toResource
      _ <- (
        assertIO(p.getName.pure[F], Field.QuantityDescriptor) &>
        assertIO(p.isMandatory.pure[F], true) &>
        assertIO(p.isReadonly.pure[F], true) &>
        assertIO(p.isNotNull.pure[F], true) &>
        assertIO(p.getType.pure[F], OType.INTEGER)
      ).toResource

      p <- oClass.getProperty(Field.Fungibility).pure[F].toResource
      _ <- (
        assertIO(p.getName.pure[F], Field.Fungibility) &>
        assertIO(p.isMandatory.pure[F], true) &>
        assertIO(p.isReadonly.pure[F], true) &>
        assertIO(p.isNotNull.pure[F], true) &>
        assertIO(p.getType.pure[F], OType.INTEGER)
      ).toResource

      p <- oClass.getProperty(Field.EphemeralMetadataScheme).pure[F].toResource
      _ <- (
        assertIO(p.getName.pure[F], Field.EphemeralMetadataScheme) &>
        assertIO(p.isMandatory.pure[F], true) &>
        assertIO(p.isReadonly.pure[F], true) &>
        assertIO(p.isNotNull.pure[F], true) &>
        assertIO(p.getType.pure[F], OType.BINARY)
      ).toResource

      p <- oClass.getProperty(Field.PermanentMetadataScheme).pure[F].toResource
      _ <- (
        assertIO(p.getName.pure[F], Field.PermanentMetadataScheme) &>
        assertIO(p.isMandatory.pure[F], true) &>
        assertIO(p.isReadonly.pure[F], true) &>
        assertIO(p.isNotNull.pure[F], true) &>
        assertIO(p.getType.pure[F], OType.BINARY)
      ).toResource

    } yield ()

    res.use_

  }

  orientDbFixture.test("Series Policy Schema Add vertex") { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {

      dbTx          <- oThread.delay(odbFactory.getTx).toResource
      transactionId <- BramblGenerator.arbitraryTransactionId.arbitrary.first.pure[F].toResource

      registrationUtxo = TransactionOutputAddress(0, 0, 0, transactionId)
      seriesPolicy = SeriesPolicy(label = "Crypto Frogs series", tokenSupply = None, registrationUtxo)

      vertex_test_1 <- oThread
        .delay(dbTx.addVertex(s"class:${seriesPolicySchema.name}", seriesPolicySchema.encode(seriesPolicy).asJava))
        .toResource

      _ = assert(
        vertex_test_1
          .getProperty[Array[Byte]](seriesPolicySchema.properties.filter(_.name == Field.SeriesPolicyId).head.name)
          .toSeq
          == seriesPolicy.computeId.value.toByteArray.toSeq
      )

      _ = assert(
        vertex_test_1
          .getProperty[String](seriesPolicySchema.properties.filter(_.name == Field.Label).head.name)
          == seriesPolicy.label
      )

      _ = assert(
        vertex_test_1
          .getProperty[Array[Byte]](seriesPolicySchema.properties.filter(_.name == Field.RegistrationUtxo).head.name)
          .toSeq
          ==
            seriesPolicy.registrationUtxo.toByteArray.toSeq
      )

      _ = assert(
        vertex_test_1
          .getProperty[Int](seriesPolicySchema.properties.filter(_.name == Field.QuantityDescriptor).head.name)
          ==
            seriesPolicy.quantityDescriptor.value
      )

      _ = assert(
        vertex_test_1
          .getProperty[Int](seriesPolicySchema.properties.filter(_.name == Field.Fungibility).head.name)
          ==
            seriesPolicy.fungibility.value
      )

      _ = assert(
        vertex_test_1
          .getProperty[Array[Byte]](
            seriesPolicySchema.properties.filter(_.name == Field.EphemeralMetadataScheme).head.name
          )
          .toSeq
          == seriesPolicy.ephemeralMetadataScheme.map(_.toByteArray).getOrElse(Array.empty).toSeq
      )

      _ = assert(
        vertex_test_1
          .getProperty[Array[Byte]](
            seriesPolicySchema.properties.filter(_.name == Field.PermanentMetadataScheme).head.name
          )
          .toSeq
          == seriesPolicy.permanentMetadataScheme.map(_.toByteArray).getOrElse(Array.empty).toSeq
      )

    } yield ()
    res.use_

  }
}
