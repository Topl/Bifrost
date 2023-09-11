package co.topl.genusLibrary.orientDb.instances

import cats.implicits._
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.generators.{ModelGenerators => BramblGenerator}
import co.topl.brambl.syntax.seriesPolicyAsSeriesPolicySyntaxOps
import co.topl.genusLibrary.DbFixtureUtilV2
import co.topl.genusLibrary.orientDb.OrientThread
import com.orientechnologies.orient.core.metadata.schema.OType
import co.topl.genusLibrary.orientDb.instances.SchemaGroupPolicy.Field
import co.topl.brambl.syntax.groupPolicyAsGroupPolicySyntaxOps
import co.topl.genusLibrary.orientDb.instances.VertexSchemaInstances.instances.groupPolicySchema
import munit.{CatsEffectFunFixtures, CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import co.topl.models.ModelGenerators.GenHelper
import scala.jdk.CollectionConverters._

class SchemaGroupPolicyTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with CatsEffectFunFixtures
    with DbFixtureUtilV2 {

  orientDbFixtureV2.test("GroupPolicy Schema Metadata") { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {
      dbNoTx             <- oThread.delay(odbFactory.getNoTx).toResource
      databaseDocumentTx <- oThread.delay(dbNoTx.getRawGraph).toResource
      oClass             <- oThread.delay(databaseDocumentTx.getClass(Field.SchemaName)).toResource

      _ <- assertIO(oClass.getName.pure[F], Field.SchemaName, s"${Field.SchemaName} Class was not created").toResource

      labelProperty <- oClass.getProperty(Field.Label).pure[F].toResource
      _ <- (
        assertIO(labelProperty.getName.pure[F], Field.Label) &>
        assertIO(labelProperty.isMandatory.pure[F], true) &>
        assertIO(labelProperty.isReadonly.pure[F], true) &>
        assertIO(labelProperty.isNotNull.pure[F], true) &>
        assertIO(labelProperty.getType.pure[F], OType.STRING)
      ).toResource

      registrationUtxo <- oClass.getProperty(Field.RegistrationUtxo).pure[F].toResource
      _ <- (
        assertIO(registrationUtxo.getName.pure[F], Field.RegistrationUtxo) &>
        assertIO(registrationUtxo.isMandatory.pure[F], true) &>
        assertIO(registrationUtxo.isReadonly.pure[F], true) &>
        assertIO(registrationUtxo.isNotNull.pure[F], true) &>
        assertIO(registrationUtxo.getType.pure[F], OType.BINARY)
      ).toResource

      fixedSeriesProperty <- oClass.getProperty(Field.FixedSeries).pure[F].toResource
      _ <- (
        assertIO(fixedSeriesProperty.getName.pure[F], Field.FixedSeries) &>
        assertIO(fixedSeriesProperty.isMandatory.pure[F], false) &>
        assertIO(fixedSeriesProperty.isReadonly.pure[F], true) &>
        assertIO(fixedSeriesProperty.isNotNull.pure[F], false) &>
        assertIO(fixedSeriesProperty.getType.pure[F], OType.BINARY)
      ).toResource

    } yield ()

    res.use_

  }

  orientDbFixtureV2.test("GroupPolicy Schema Add vertex") { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {

      dbTx          <- oThread.delay(odbFactory.getTx).toResource
      transactionId <- BramblGenerator.arbitraryTransactionId.arbitrary.first.pure[F].toResource

      registrationUtxo = TransactionOutputAddress(0, 0, 0, transactionId)
      seriesPolicy = SeriesPolicy(label = "Crypto Frogs series", tokenSupply = None, registrationUtxo)
      groupPolicy = GroupPolicy(label = "Crypto Frogs", registrationUtxo, Some(seriesPolicy.computeId))

      vertex_test_1 <- oThread
        .delay(dbTx.addVertex(s"class:${groupPolicySchema.name}", groupPolicySchema.encode(groupPolicy).asJava))
        .toResource

      _ = assert(
        vertex_test_1
          .getProperty[Array[Byte]](groupPolicySchema.properties.filter(_.name == Field.GroupPolicyId).head.name)
          .toSeq
          == groupPolicy.computeId.value.toByteArray.toSeq
      )

      _ = assert(
        vertex_test_1
          .getProperty[String](groupPolicySchema.properties.filter(_.name == Field.Label).head.name)
          == groupPolicy.label
      )

      _ = assert(
        vertex_test_1
          .getProperty[Array[Byte]](groupPolicySchema.properties.filter(_.name == Field.RegistrationUtxo).head.name)
          .toSeq
          ==
            groupPolicy.registrationUtxo.toByteArray.toSeq
      )

      _ = assert(
        vertex_test_1
          .getProperty[Array[Byte]](groupPolicySchema.properties.filter(_.name == Field.FixedSeries).head.name)
          .toSeq
          ==
            groupPolicy.fixedSeries.get.value.toByteArray.toSeq
      )

      groupPolicyDecoded = groupPolicySchema.decodeVertex(vertex_test_1)
      _ = assert(groupPolicyDecoded == groupPolicy)

      registrationUtxo = TransactionOutputAddress(1, 1, 1, transactionId)
      groupPolicyWithNoneSeries = GroupPolicy(label = "Crypto Frogs with None fixed series", registrationUtxo, None)

      vertex_test_2 <- oThread
        .delay(
          dbTx.addVertex(s"class:${groupPolicySchema.name}", groupPolicySchema.encode(groupPolicyWithNoneSeries).asJava)
        )
        .toResource

      _ = assert(
        vertex_test_2
          .getProperty[Array[Byte]](groupPolicySchema.properties.filter(_.name == Field.GroupPolicyId).head.name)
          .toSeq
          == groupPolicyWithNoneSeries.computeId.value.toByteArray.toSeq
      )

      _ = assert(
        vertex_test_2
          .getProperty[String](groupPolicySchema.properties.filter(_.name == Field.Label).head.name)
          == groupPolicyWithNoneSeries.label
      )

      _ = assert(
        vertex_test_2
          .getProperty[Array[Byte]](groupPolicySchema.properties.filter(_.name == Field.RegistrationUtxo).head.name)
          .toSeq ==
          groupPolicyWithNoneSeries.registrationUtxo.toByteArray.toSeq
      )

      _ = assert(
        vertex_test_2
          .getProperty[Array[Byte]](groupPolicySchema.properties.filter(_.name == Field.FixedSeries).head.name)
          .toSeq ==
          Array.empty[Byte].toSeq
      )

      groupPolicyDecoded = groupPolicySchema.decodeVertex(vertex_test_2)
      _ = assert(groupPolicyDecoded == groupPolicyWithNoneSeries)

    } yield ()
    res.use_

  }
}
