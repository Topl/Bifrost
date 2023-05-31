package co.topl.genusLibrary.interpreter

import cats.effect.implicits.effectResourceOps
import cats.implicits._
import co.topl.brambl.generators.{ModelGenerators => BramblGenerator}
import co.topl.genus.services.BlockchainSizeStats
import co.topl.genusLibrary.DbFixtureUtilV2
import co.topl.genusLibrary.model.GE
import co.topl.genusLibrary.orientDb.instances.{SchemaBlockHeader, SchemaIoTransaction}
import co.topl.genusLibrary.orientDb.instances.VertexSchemaInstances.instances.{blockHeaderSchema, ioTransactionSchema}
import co.topl.genusLibrary.orientDb.{OrientDBMetadataFactory, OrientThread}
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import scala.jdk.CollectionConverters._

class GraphVertexFetcherV2Test
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with DbFixtureUtilV2 {

  orientDbFixtureV2.test(
    "On fetchBlockchainSizeStats, after saving a blockheader and an IoTransaction, BlockchainSizeStats should be returned with sizes"
  ) { case (odbFactory, oThread) =>
    val res = for {
      implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]

      databaseDocumentTx <- oThread.delay(odbFactory.getNoTx.getRawGraph).toResource

      _ <- Seq(
        blockHeaderSchema,
        ioTransactionSchema
      )
        .traverse(OrientDBMetadataFactory.createVertex[F](databaseDocumentTx, _))
        .void
        .toResource

      blockHeader   <- ModelGenerators.arbitraryHeader.arbitrary.first.pure[F].toResource
      ioTransaction <- BramblGenerator.arbitraryIoTransaction.arbitrary.first.pure[F].toResource

      _ <- oThread
        .delay(odbFactory.getTx)
        .map { tx =>
          tx.addVertex(s"class:${blockHeaderSchema.name}", blockHeaderSchema.encode(blockHeader).asJava)
          tx.commit()
        }
        .toResource

      _ <- orientThread
        .delay(odbFactory.getTx)
        .map { tx =>
          tx.addVertex(s"class:${ioTransactionSchema.name}", ioTransactionSchema.encode(ioTransaction).asJava)
          tx.commit()
        }
        .toResource

      graphVertexFetcher <- GraphVertexFetcher.make[F](odbFactory.getNoTx)
      _ <- assertIO(
        graphVertexFetcher.fetchBlockchainSizeStats(),
        BlockchainSizeStats.defaultInstance
          .withBlockHeaderBytes(SchemaBlockHeader.size(blockHeader))
          .withTransactionBytes(SchemaIoTransaction.size(ioTransaction))
          .asRight[GE]
      ).toResource
    } yield ()

    res.use_

  }

}
