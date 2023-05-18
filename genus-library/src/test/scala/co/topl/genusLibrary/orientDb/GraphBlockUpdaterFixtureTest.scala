package co.topl.genusLibrary.orientDb

import cats.effect.implicits.effectResourceOps
import cats.implicits.toTraverseOps
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.consensus.models.BlockHeader
import co.topl.genus.services.BlockData
import co.topl.genusLibrary.DbFixtureUtil
import co.topl.genusLibrary.algebras.{BlockFetcherAlgebra, NodeBlockFetcherAlgebra}
import co.topl.genusLibrary.interpreter.GraphBlockUpdater
import co.topl.genusLibrary.orientDb.instances.VertexSchemaInstances.instances._
import co.topl.genusLibrary.orientDb.schema.EdgeSchemaInstances._
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models.generators.node.ModelGenerators._
import co.topl.node.models.BlockBody
import com.tinkerpop.blueprints.impls.orient.OrientGraphFactoryV2
import fs2.Stream
import java.util.concurrent.TimeUnit
import munit.{CatsEffectFunFixtures, CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.jdk.CollectionConverters.IterableHasAsScala

class GraphBlockUpdaterFixtureTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with CatsEffectFunFixtures
    with DbFixtureUtil {

  orientDbFixture.test("Insert and remove genesis block") { case (odb, oThread) =>
    PropF.forAllF { (blockHeader: BlockHeader, blockBody: BlockBody, ioTransaction: IoTransaction) =>
      withMock {
        val nodeBlockFetcher = mock[NodeBlockFetcherAlgebra[F, Stream[F, *]]]
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val res = for {
          implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
          odbFactory <- oThread.delay(new OrientGraphFactoryV2(odb, "testDb", "testUser", "testPass")).toResource
          dbNoTx     <- oThread.delay(odbFactory.getNoTx).toResource
          _          <- oThread.delay(dbNoTx.makeActive()).toResource

          databaseDocumentTx <- oThread.delay(odbFactory.getNoTx.getRawGraph).toResource

          _ <- Seq(
            blockHeaderSchema,
            blockBodySchema,
            ioTransactionSchema,
            canonicalHeadSchema,
            lockAddressSchema,
            txoSchema
          )
            .traverse(OrientDBMetadataFactory.createVertex[F](databaseDocumentTx, _))
            .void
            .toResource

          _ <- Seq(blockHeaderEdge, blockHeaderBodyEdge, blockHeaderTxIOEdge, addressTxIOEdge, addressTxoEdge)
            .traverse(e => OrientDBMetadataFactory.createEdge[F](databaseDocumentTx, e))
            .void
            .toResource

          blockData = BlockData(
            blockHeader.copy(height = 1),
            blockBody,
            transactions = Seq(ioTransaction.embedId)
          )

          dbTx <- oThread.delay(odbFactory.getTx).toResource

          graphBlockUpdater <- GraphBlockUpdater.make[F](dbTx, blockFetcher, nodeBlockFetcher)
          _                 <- graphBlockUpdater.insert(blockData).toResource

          blockHeaderVertex = dbTx.getHeader(blockData.header)
          _ = assert(blockHeaderVertex.isDefined)
          _ <- assertIOBoolean(oThread.delay(dbTx.getHeader(blockData.header).isDefined)).toResource
          _ <- assertIOBoolean(oThread.delay(dbTx.getBody(blockHeaderVertex.get).isDefined)).toResource

          _ <- graphBlockUpdater.remove(blockData).toResource

          // When we remove headerVertex, the canonicalHead schema is not updated, insertions handle it
          // When we remove txoVertex, lockAddress schema is not updated, because lockAddress references many txo
          // Eventually could create an orphan lockAddress, it is not a problem cause some future txo will reference it
          blockHeaderVertex = dbTx.getHeader(blockData.header)
          _ = assert(blockHeaderVertex.isEmpty)
          _ <- assertIOBoolean(oThread.delay(dbTx.getHeader(blockData.header).isEmpty)).toResource
          _ <- assertIOBoolean(oThread.delay(dbTx.getVerticesOfClass(blockBodySchema.name).asScala.isEmpty)).toResource
          _ <- assertIOBoolean(
            oThread.delay(dbTx.getVerticesOfClass(ioTransactionSchema.name).asScala.isEmpty)
          ).toResource
          _ <- assertIOBoolean(oThread.delay(dbTx.getVerticesOfClass(txoSchema.name).asScala.isEmpty)).toResource

        } yield ()

        res.use_
      }
    }
  }

  override def munitTimeout: Duration = new FiniteDuration(60, TimeUnit.SECONDS)
}
