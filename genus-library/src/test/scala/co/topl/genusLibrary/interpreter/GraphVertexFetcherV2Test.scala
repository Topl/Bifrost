package co.topl.genusLibrary.interpreter

import cats.data.EitherT
import cats.effect.implicits.effectResourceOps
import cats.implicits._
import co.topl.brambl.generators.{ModelGenerators => BramblGenerator}
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.genus.services.{BlockStats, BlockchainSizeStats, Txo, TxoState, TxoStats}
import co.topl.genusLibrary.DbFixtureUtilV2
import co.topl.genusLibrary.model.GE
import co.topl.genusLibrary.orientDb.OrientThread
import co.topl.genusLibrary.orientDb.instances.VertexSchemaInstances.instances.{blockBodySchema, Ops}
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.genusLibrary.orientDb.instances.{SchemaBlockHeader, SchemaIoTransaction}
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators
import co.topl.node.models.BlockBody
import com.orientechnologies.orient.core.id.ORecordId
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.OrientVertex
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory

class GraphVertexFetcherV2Test
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with DbFixtureUtilV2 {

  orientDbFixtureV2.test("On fetchBlockHeader, None should be returned") {
    case (odbFactory, implicit0(oThread: OrientThread[F])) =>
      val res = for {
        blockHeader        <- ModelGenerators.arbitraryHeader.arbitrary.first.pure[F].toResource
        notx               <- oThread.delay(odbFactory.getNoTx).toResource
        graphVertexFetcher <- GraphVertexFetcher.make[F](notx)
        _ <- assertIO(
          graphVertexFetcher.fetchHeader(blockHeader.id),
          Option.empty[Vertex].asRight[GE]
        ).toResource
      } yield ()

      res.use_
  }

  orientDbFixtureV2.test("On fetchBlockHeader, a blockHeader should be returned") {
    case (odbFactory, implicit0(oThread: OrientThread[F])) =>
      val res = for {
        blockHeader        <- ModelGenerators.arbitraryHeader.arbitrary.first.pure[F].toResource
        tx                 <- oThread.delay(odbFactory.getTx).toResource
        notx               <- oThread.delay(odbFactory.getNoTx).toResource
        graphVertexFetcher <- GraphVertexFetcher.make[F](notx)

        _ <- oThread.delay {
          tx.addBlockHeader(blockHeader)
          tx.commit()
          tx.shutdown()
        }.toResource

        blockHeaderVertex <- graphVertexFetcher.fetchHeader(blockHeader.id).rethrow.toResource
        _                 <- assertIOBoolean(blockHeaderVertex.isDefined.pure[F]).toResource
      } yield ()

      res.use_
  }

  orientDbFixtureV2.test("On fetchHeaderByHeight, a blockHeader should be returned") {
    case (odbFactory, implicit0(oThread: OrientThread[F])) =>
      val res = for {
        blockHeader        <- ModelGenerators.arbitraryHeader.arbitrary.first.copy(height = 1).pure[F].toResource
        tx                 <- oThread.delay(odbFactory.getTx).toResource
        notx               <- oThread.delay(odbFactory.getNoTx).toResource
        graphVertexFetcher <- GraphVertexFetcher.make[F](notx)

        _ <- oThread.delay {
          tx.addBlockHeader(blockHeader)
          tx.commit()
          tx.shutdown()
        }.toResource

        blockHeaderVertex <- graphVertexFetcher.fetchHeaderByHeight(1L).rethrow.toResource
        _                 <- assertIOBoolean(blockHeaderVertex.isDefined.pure[F]).toResource
      } yield ()

      res.use_
  }

  orientDbFixtureV2.test("On fetchHeaderByDepth, a blockHeader should be returned") {
    case (odbFactory, implicit0(oThread: OrientThread[F])) =>
      val res = for {
        blockHeader        <- ModelGenerators.arbitraryHeader.arbitrary.first.copy(height = 1).pure[F].toResource
        tx                 <- oThread.delay(odbFactory.getTx).toResource
        notx               <- oThread.delay(odbFactory.getNoTx).toResource
        graphVertexFetcher <- GraphVertexFetcher.make[F](notx)

        _ <- oThread.delay {
          tx.addBlockHeader(blockHeader)
          tx.commit()
          tx.shutdown()
        }.toResource

        // depth = 0, if we have 1 vertex with height = 1, 1-0=1. It will return a vertex with height=1
        blockHeaderVertex <- graphVertexFetcher.fetchHeaderByDepth(0L).rethrow.toResource
        _                 <- assertIOBoolean(blockHeaderVertex.isDefined.pure[F]).toResource
      } yield ()

      res.use_
  }

  orientDbFixtureV2.test("On fetchBody, None should be returned") {
    case (odbFactory, implicit0(oThread: OrientThread[F])) =>
      val res = for {
        notx <- oThread.delay(odbFactory.getNoTx).toResource

        blockHeaderVertex = new OrientVertex(notx, new ORecordId(1, 1))

        graphVertexFetcher <- GraphVertexFetcher.make[F](notx)
        _ <- assertIO(graphVertexFetcher.fetchBody(blockHeaderVertex), Option.empty[Vertex].asRight[GE]).toResource
      } yield ()

      res.use_

  }

  orientDbFixtureV2.test("On fetchBody, Body should be returned") {
    case (odbFactory, implicit0(oThread: OrientThread[F])) =>
      val res = for {
        tx   <- oThread.delay(odbFactory.getTx).toResource
        notx <- oThread.delay(odbFactory.getNoTx).toResource

        blockHeader <- ModelGenerators.arbitraryHeader.arbitrary.first.pure[F].toResource

        blockHeaderVertex <- oThread.delay {
          val blockHeaderVertex = tx.addBlockHeader(blockHeader)
          // TODO add body Vertex should set the property inside the ops method
          val bodyVertex = tx.addBody(BlockBody(Seq.empty))
          bodyVertex.setProperty(blockBodySchema.links.head.propertyName, blockHeaderVertex.getId)
          tx.commit()
          tx.shutdown()
          blockHeaderVertex
        }.toResource

        graphVertexFetcher <- GraphVertexFetcher.make[F](notx)

        bodyVertex <- graphVertexFetcher.fetchBody(blockHeaderVertex).rethrow.toResource
        _          <- assertIOBoolean(bodyVertex.isDefined.pure[F]).toResource
      } yield ()

      res.use_

  }

  orientDbFixtureV2.test("On fetchTransactions, None should be returned") {
    case (odbFactory, implicit0(oThread: OrientThread[F])) =>
      val res = for {
        notx <- oThread.delay(odbFactory.getNoTx).toResource

        blockHeaderVertex = new OrientVertex(notx, new ORecordId(1, 1))

        graphVertexFetcher <- GraphVertexFetcher.make[F](notx)
        _ <- assertIO(
          EitherT(graphVertexFetcher.fetchTransactions(blockHeaderVertex)).map(_.size).value,
          0.asRight[GE]
        ).toResource
      } yield ()

      res.use_

  }

  orientDbFixtureV2.test("On fetchLockAddress, None should be returned") {
    case (odbFactory, implicit0(oThread: OrientThread[F])) =>
      val res = for {
        lockAddress        <- BramblGenerator.arbitraryLockAddress.arbitrary.first.pure[F].toResource
        notx               <- oThread.delay(odbFactory.getNoTx).toResource
        graphVertexFetcher <- GraphVertexFetcher.make[F](notx)

        _ <- assertIO(
          graphVertexFetcher.fetchLockAddress(lockAddress),
          Option.empty[Vertex].asRight[GE]
        ).toResource
      } yield ()

      res.use_
  }

  orientDbFixtureV2.test("On fetchLockAddress, a LockAddress should be returned") {
    case (odbFactory, implicit0(oThread: OrientThread[F])) =>
      val res = for {
        lockAddress        <- BramblGenerator.arbitraryLockAddress.arbitrary.first.pure[F].toResource
        tx                 <- oThread.delay(odbFactory.getTx).toResource
        notx               <- oThread.delay(odbFactory.getNoTx).toResource
        graphVertexFetcher <- GraphVertexFetcher.make[F](notx)

        _ <- oThread.delay {
          tx.addLockAddress(lockAddress)
          tx.commit()
          tx.shutdown()
        }.toResource

        lockAddressVertex <- graphVertexFetcher.fetchLockAddress(lockAddress).rethrow.toResource
        _                 <- assertIOBoolean(lockAddressVertex.isDefined.pure[F]).toResource
      } yield ()

      res.use_

  }

  orientDbFixtureV2.test("On fetchTxo, None should be returned") {
    case (odbFactory, implicit0(oThread: OrientThread[F])) =>
      val res = for {
        transactionOutputAddress <- BramblGenerator.arbitraryTransactionOutputAddress.arbitrary.first.pure[F].toResource
        notx                     <- oThread.delay(odbFactory.getNoTx).toResource
        graphVertexFetcher       <- GraphVertexFetcher.make[F](notx)

        _ <- assertIO(
          graphVertexFetcher.fetchTxo(transactionOutputAddress),
          Option.empty[Vertex].asRight[GE]
        ).toResource
      } yield ()

      res.use_

  }

  orientDbFixtureV2.test("On fetchTxo, Txo should be returned") {
    case (odbFactory, implicit0(oThread: OrientThread[F])) =>
      val res = for {
        transactionOutputAddress <- BramblGenerator.arbitraryTransactionOutputAddress.arbitrary.first.pure[F].toResource
        unspentTransactionOutput <- BramblGenerator.arbitraryUnspentTransactionOutput.arbitrary.first.pure[F].toResource

        tx                 <- oThread.delay(odbFactory.getTx).toResource
        notx               <- oThread.delay(odbFactory.getNoTx).toResource
        graphVertexFetcher <- GraphVertexFetcher.make[F](notx)

        _ <- oThread.delay {
          tx.addTxo(Txo(unspentTransactionOutput, TxoState.SPENT, transactionOutputAddress))
          tx.commit()
          tx.shutdown()
        }.toResource

        txoVertex <- graphVertexFetcher.fetchTxo(transactionOutputAddress).rethrow.toResource
        _         <- assertIOBoolean(txoVertex.isDefined.pure[F]).toResource
      } yield ()

      res.use_

  }

  orientDbFixtureV2.test(
    "On fetchTxoStats, a default instance of TxoStats should be returned"
  ) { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {
      graphVertexFetcher <- GraphVertexFetcher.make[F](odbFactory.getNoTx)
      _ <- assertIO(graphVertexFetcher.fetchTxoStats(), TxoStats.defaultInstance.asRight[GE]).toResource
    } yield ()

    res.use_
  }

  orientDbFixtureV2.test(
    "On fetchTxoStats, TxoStats SPENT should be returned"
  ) { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {

      transactionOutputAddress <- BramblGenerator.arbitraryTransactionOutputAddress.arbitrary.first.pure[F].toResource
      unspentTransactionOutput <- BramblGenerator.arbitraryUnspentTransactionOutput.arbitrary.first.pure[F].toResource

      tx <- oThread.delay(odbFactory.getTx).toResource

      _ <- oThread.delay {
        tx.addTxo(Txo(unspentTransactionOutput, TxoState.SPENT, transactionOutputAddress))
        tx.commit()
        tx.shutdown()
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
    "On fetchTxoStats, TxoStats UNSPENT should be returned"
  ) { case (odbFactory, implicit0(oThread: OrientThread[F])) =>
    val res = for {

      transactionOutputAddress <- BramblGenerator.arbitraryTransactionOutputAddress.arbitrary.first.pure[F].toResource
      unspentTransactionOutput <- BramblGenerator.arbitraryUnspentTransactionOutput.arbitrary.first.pure[F].toResource

      tx <- oThread.delay(odbFactory.getTx).toResource

      _ <- oThread.delay {
        tx.addTxo(Txo(unspentTransactionOutput, TxoState.UNSPENT, transactionOutputAddress))
        tx.commit()
        tx.shutdown()
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
    "On fetchBlockchainSizeStats, a default instance of BlockchainSizeStats should be returned"
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
          tx.addBlockHeader(blockHeader)
          tx.commit()
        }
        .toResource

      _ <- oThread
        .delay(odbFactory.getTx)
        .map { tx =>
          tx.addIoTx(ioTransaction)
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

  orientDbFixtureV2.test("On fetchBlockStats, BodyStats with 1 empty result should be returned") {
    case (odbFactory, implicit0(oThread: OrientThread[F])) =>
      val res = for {
        tx   <- oThread.delay(odbFactory.getTx).toResource
        notx <- oThread.delay(odbFactory.getNoTx).toResource

        blockHeader <- ModelGenerators.arbitraryHeader.arbitrary.first.pure[F].toResource

        _ <- oThread.delay {
          val blockHeaderVertex = tx.addBlockHeader(blockHeader)
          val bodyVertex = tx.addBody(BlockBody(Seq.empty))
          bodyVertex.setProperty(blockBodySchema.links.head.propertyName, blockHeaderVertex.getId)
          tx.commit()
          tx.shutdown()
        }.toResource

        graphVertexFetcher <- GraphVertexFetcher.make[F](notx)

        _ <- assertIO(graphVertexFetcher.fetchBlockStats().rethrow, BlockStats(empty = 1, nonEmpty = 0)).toResource
      } yield ()

      res.use_

  }

  orientDbFixtureV2.test("On fetchBlockStats, BodyStats with 1 non empty result should be returned") {
    case (odbFactory, implicit0(oThread: OrientThread[F])) =>
      val res = for {
        tx   <- oThread.delay(odbFactory.getTx).toResource
        notx <- oThread.delay(odbFactory.getNoTx).toResource

        blockHeader   <- ModelGenerators.arbitraryHeader.arbitrary.first.pure[F].toResource
        ioTransaction <- BramblGenerator.arbitraryIoTransaction.arbitrary.first.pure[F].toResource

        _ <- oThread.delay {
          val blockHeaderVertex = tx.addBlockHeader(blockHeader)
          val bodyVertex = tx.addBody(BlockBody(Seq(ioTransaction.id)))
          bodyVertex.setProperty(blockBodySchema.links.head.propertyName, blockHeaderVertex.getId)
          tx.commit()
          tx.shutdown()
        }.toResource

        graphVertexFetcher <- GraphVertexFetcher.make[F](notx)

        _ <- assertIO(graphVertexFetcher.fetchBlockStats().rethrow, BlockStats(empty = 0, nonEmpty = 1)).toResource
      } yield ()

      res.use_

  }
}
