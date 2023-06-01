package co.topl.genusLibrary.interpreter

import cats.effect.implicits.effectResourceOps
import cats.implicits._
import co.topl.brambl.generators.{ModelGenerators => BramblGenerator}
import co.topl.genus.services.{BlockchainSizeStats, Txo, TxoState, TxoStats}
import co.topl.genusLibrary.DbFixtureUtilV2
import co.topl.genusLibrary.model.GE
import co.topl.genusLibrary.orientDb.OrientThread
import co.topl.genusLibrary.orientDb.instances.VertexSchemaInstances.instances.{
  blockHeaderSchema,
  ioTransactionSchema,
  txoSchema,
  Ops
}
import co.topl.genusLibrary.orientDb.instances.{SchemaBlockHeader, SchemaIoTransaction}
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
    "On fetchTxoStats if an empty iterator is returned, a default instance of TxoStats should be returned"
  ) { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {
      graphVertexFetcher <- GraphVertexFetcher.make[F](odbFactory.getNoTx)
      _ <- assertIO(graphVertexFetcher.fetchTxoStats(), TxoStats.defaultInstance.asRight[GE]).toResource
    } yield ()

    res.use_
  }

  orientDbFixtureV2.test(
    "On fetchTxoStats if an empty iterator is returned, TxoStats SPENT should be returned"
  ) { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {

      transactionOutputAddress <- BramblGenerator.arbitraryTransactionOutputAddress.arbitrary.first.pure[F].toResource
      unspentTransactionOutput <- BramblGenerator.arbitraryUnspentTransactionOutput.arbitrary.first.pure[F].toResource

      tx   <- oThread.delay(odbFactory.getTx).toResource
      notx <- oThread.delay(odbFactory.getNoTx).toResource

      _ <- oThread.delay {
        tx.addTxo(Txo(unspentTransactionOutput, TxoState.SPENT, transactionOutputAddress))
        tx.commit()
        tx.shutdown()
      }.toResource

      _ <- oThread.delay {
        notx.getVerticesOfClass(s"${txoSchema.name}").asScala.foreach(v => println(v.getProperty("state")))
      }.toResource

      graphVertexFetcher <- GraphVertexFetcher.make[F](odbFactory.getNoTx)
      _ <- assertIO(
        graphVertexFetcher.fetchTxoStats(),
        TxoStats.defaultInstance
          .withSpent(1)
          .withTotal(1)
          .asRight[GE]
      ).toResource
    } yield ()
    res.use_
  }

  orientDbFixtureV2.test(
    "On fetchTxoStats if an empty iterator is returned, TxoStats UNSPENT should be returned"
  ) { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {

      transactionOutputAddress <- BramblGenerator.arbitraryTransactionOutputAddress.arbitrary.first.pure[F].toResource
      unspentTransactionOutput <- BramblGenerator.arbitraryUnspentTransactionOutput.arbitrary.first.pure[F].toResource

      tx   <- oThread.delay(odbFactory.getTx).toResource
      notx <- oThread.delay(odbFactory.getNoTx).toResource

      _ <- oThread.delay {
        tx.addTxo(Txo(unspentTransactionOutput, TxoState.UNSPENT, transactionOutputAddress))
        tx.commit()
        tx.shutdown()
      }.toResource

      _ <- oThread.delay {
        notx.getVerticesOfClass(s"${txoSchema.name}").asScala.foreach(v => println(v.getProperty("state")))
      }.toResource

      graphVertexFetcher <- GraphVertexFetcher.make[F](odbFactory.getNoTx)
      _ <- assertIO(
        graphVertexFetcher.fetchTxoStats(),
        TxoStats.defaultInstance
          .withUnspent(1)
          .withTotal(1)
          .asRight[GE]
      ).toResource
    } yield ()
    res.use_
  }

  orientDbFixtureV2.test(
    "On fetchBlockchainSizeStats if an empty iterator is returned in both cases, a default instance of BlockchainSizeStats should be returned"
  ) { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {

      dbNoTx <- oThread.delay(odbFactory.getNoTx).toResource

      graphVertexFetcher <- GraphVertexFetcher.make[F](dbNoTx)
      _ <- assertIO(
        graphVertexFetcher.fetchBlockchainSizeStats(),
        BlockchainSizeStats.defaultInstance.asRight[GE]
      ).toResource
    } yield ()

    res.use_
  }

  orientDbFixtureV2.test(
    "On fetchBlockchainSizeStats, after saving a blockheader and an IoTransaction, BlockchainSizeStats should be returned with sizes"
  ) { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {

      blockHeader   <- ModelGenerators.arbitraryHeader.arbitrary.first.pure[F].toResource
      ioTransaction <- BramblGenerator.arbitraryIoTransaction.arbitrary.first.pure[F].toResource

      _ <- oThread
        .delay(odbFactory.getTx)
        .map { tx =>
          tx.addVertex(s"class:${blockHeaderSchema.name}", blockHeaderSchema.encode(blockHeader).asJava)
          tx.commit()
        }
        .toResource

      _ <- oThread
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
