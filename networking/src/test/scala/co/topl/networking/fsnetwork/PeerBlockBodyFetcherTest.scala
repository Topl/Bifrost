package co.topl.networking.fsnetwork

import cats.effect.IO
import cats.implicits._
import co.topl.algebras.Store
import co.topl.brambl.generators.TransactionGenerator
import co.topl.brambl.models.Identifier
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances.ioTransactionAsIoTransactionOps
import co.topl.consensus.models.BlockId
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators.nonEmptyChainArbOf
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.PeerBlockHeaderFetcherTest.F
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.networking.fsnetwork.TestHelper.{CallHandler1Ops, CallHandler2Ops}
import co.topl.node.models.BlockBody
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.mutable

object PeerBlockBodyFetcherTest {
  type F[A] = IO[A]
}

class PeerBlockBodyFetcherTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with TransactionGenerator {
  implicit val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  val hostId: HostId = "127.0.0.1"
  val maxChainSize = 99

  test("Block bodies shall return error if block is not present on client") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val transactionStore = mock[Store[F, Identifier.IoTransaction32, IoTransaction]]

      val (txs, bodies) =
        nonEmptyChainArbOf(TestHelper.arbitraryTxsAndBlock).arbitrary
          .retryUntil(c => c.size > 1 && c.size < maxChainSize)
          .first
          .unzip

      val blockIdsAndBodies =
        bodies.map(b => (co.topl.models.generators.consensus.ModelGenerators.arbitraryBlockId.arbitrary.first, b))

      def blockIsMissed(id: BlockId): Boolean = id.hashCode() % 2 == 0

      val (presentBlockIdAndBodies, _) =
        blockIdsAndBodies.toList.partition { case (id, _) => blockIsMissed(id) }

      val txIdsAndTxs = txs.toList.flatten.map(tx => (tx.id, tx))
      val (missedTxs, presentTxs) = txIdsAndTxs.partition { case (id, _) => id.hashCode() % 2 == 0 }

      val clientBodiesData = presentBlockIdAndBodies.toMap
      (client.getRemoteBody _).expects(*).rep(blockIdsAndBodies.size.toInt).onCall { id: BlockId =>
        clientBodiesData.get(id).pure[F]
      }

      val transactionStoreData = presentTxs.toMap
      (transactionStore.contains _).expects(*).anyNumberOfTimes().onCall { id: Identifier.IoTransaction32 =>
        transactionStoreData.contains(id).pure[F]
      }

      val clientTxsData = missedTxs.toMap
      (client.getRemoteTransaction _).expects(*).anyNumberOfTimes().onCall { id: Identifier.IoTransaction32 =>
        clientTxsData.get(id).pure[F]
      }

      val downloadedTxs =
        mutable.Map.empty[Identifier.IoTransaction32, IoTransaction]
      (transactionStore.put _).expects(*, *).anyNumberOfTimes().onCall {
        case (id: Identifier.IoTransaction32, tx: IoTransaction) =>
          downloadedTxs.put(id, tx).pure[F].void
      }

      val wrappedBodies =
        blockIdsAndBodies.map { case (id, body) =>
          if (clientBodiesData.contains(id)) {
            (id, Either.right[BlockBodyDownloadError, BlockBody](body))
          } else {
            (id, Either.left[BlockBodyDownloadError, BlockBody](BlockBodyDownloadError.BodyNotFoundInPeer))
          }
        }

      val expectedMessage = RequestsProxy.Message.DownloadBlockResponse(hostId, wrappedBodies)
      (requestsProxy.sendNoWait _).expects(expectedMessage).once().returning(().pure[F])

      PeerBlockBodyFetcher
        .makeActor(hostId, client, requestsProxy, transactionStore)
        .use { actor =>
          for {
            _ <- actor.send(PeerBlockBodyFetcher.Message.DownloadBlocks(blockIdsAndBodies.unzip._1))
            _ = assert(downloadedTxs.size <= missedTxs.toMap.size)
          } yield ()
        }
    }
  }

  test("Block bodies shall return error if client has not transaction") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val transactionStore = mock[Store[F, Identifier.IoTransaction32, IoTransaction]]

      val transactionsAndBody =
        nonEmptyChainArbOf(TestHelper.arbitraryTxsAndBlock).arbitrary
          .retryUntil(c => c.size > 1 && c.size < maxChainSize)
          .first

      val idBodyTxIdTx =
        transactionsAndBody
          .map { case (txs, body) =>
            val id = co.topl.models.generators.consensus.ModelGenerators.arbitraryBlockId.arbitrary.first
            (id, body, txs.map(tx => (tx.id, tx)))
          }

      val idAndBody = idBodyTxIdTx.map(d => (d._1, d._2))

      def transactionIsMissed(id:  Identifier.IoTransaction32): Boolean = id.hashCode() % 7 == 0
      def blockIsMissed(blockBody: BlockBody): Boolean = blockBody.transactionIds.exists(transactionIsMissed)

      val presentBlockIdAndBodies = idAndBody.toList

      val txIdsAndTxs =
        idBodyTxIdTx.map(d => d._3).toList.flatten

      val clientBodiesData = presentBlockIdAndBodies.toMap
      (client.getRemoteBody _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        clientBodiesData.get(id).pure[F]
      }

      (transactionStore.contains _).expects(*).anyNumberOfTimes().returning(false.pure[F])

      val clientTxsData = txIdsAndTxs.toMap
      (client.getRemoteTransaction _).expects(*).anyNumberOfTimes().onCall { id: Identifier.IoTransaction32 =>
        if (transactionIsMissed(id)) {
          Option.empty[IoTransaction].pure[F]
        } else {
          clientTxsData.get(id).pure[F]
        }
      }

      (transactionStore.put _).expects(*, *).anyNumberOfTimes().returning(().pure[F])

      val wrappedBodies =
        idAndBody.map { case (id, body) =>
          if (!blockIsMissed(body)) {
            (id, Either.right[BlockBodyDownloadError, BlockBody](body))
          } else {
            val missedId = body.transactionIds.find(transactionIsMissed).get
            (
              id,
              Either.left[BlockBodyDownloadError, BlockBody](BlockBodyDownloadError.TransactionNotFoundInPeer(missedId))
            )
          }
        }

      val expectedMessage =
        RequestsProxy.Message.DownloadBlockResponse(hostId, wrappedBodies)
      (requestsProxy.sendNoWait _).expects(expectedMessage).once().returning(().pure[F])

      PeerBlockBodyFetcher
        .makeActor(hostId, client, requestsProxy, transactionStore)
        .use { actor =>
          for {
            _ <- actor.send(PeerBlockBodyFetcher.Message.DownloadBlocks(idAndBody.map(_._1)))
          } yield ()
        }
    }
  }

  test("Block bodies shall return error if client has transaction with incorrect id") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val transactionStore = mock[Store[F, Identifier.IoTransaction32, IoTransaction]]

      val transactionsAndBody =
        nonEmptyChainArbOf(TestHelper.arbitraryTxsAndBlock).arbitrary
          .retryUntil(c => c.size > 1 && c.size < maxChainSize)
          .first

      val idBodyTxIdTx =
        transactionsAndBody
          .map { case (txs, body) =>
            val id = co.topl.models.generators.consensus.ModelGenerators.arbitraryBlockId.arbitrary.first
            (id, body, txs.map(tx => (tx.id, tx)))
          }

      def transactionHaveIncorrectId(id: Identifier.IoTransaction32): Boolean = id.hashCode() % 7 == 0

      def blockIsMissed(blockBody: BlockBody): Boolean = blockBody.transactionIds.exists(transactionHaveIncorrectId)

      val incorrectTransaction = arbitraryIoTransaction.arbitrary.first
      val incorrectTransactionId = incorrectTransaction.id

      val idAndBody = idBodyTxIdTx.map(d => (d._1, d._2))

      val presentBlockIdAndBodies = idAndBody.toList

      val txIdsAndTxs =
        idBodyTxIdTx.map(d => d._3).toList.flatten

      val clientBodiesData = presentBlockIdAndBodies.toMap
      (client.getRemoteBody _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        clientBodiesData.get(id).pure[F]
      }

      (transactionStore.contains _).expects(*).anyNumberOfTimes().returning(false.pure[F])

      val clientTxsData = txIdsAndTxs.toMap
      (client.getRemoteTransaction _).expects(*).anyNumberOfTimes().onCall { id: Identifier.IoTransaction32 =>
        if (transactionHaveIncorrectId(id)) {
          Option(incorrectTransaction).pure[F]
        } else {
          clientTxsData.get(id).pure[F]
        }
      }

      (transactionStore.put _).expects(*, *).anyNumberOfTimes().returning(().pure[F])

      val wrappedBodies =
        idAndBody.map { case (id, body) =>
          if (!blockIsMissed(body)) {
            (id, Either.right[BlockBodyDownloadError, BlockBody](body))
          } else {
            val expectedId = body.transactionIds.find(transactionHaveIncorrectId).get
            (
              id,
              Either.left[BlockBodyDownloadError, BlockBody](
                BlockBodyDownloadError.TransactionHaveIncorrectId(expectedId, incorrectTransactionId)
              )
            )
          }
        }

      val expectedMessage =
        RequestsProxy.Message.DownloadBlockResponse(hostId, wrappedBodies)
      (requestsProxy.sendNoWait _).expects(expectedMessage).once().returning(().pure[F])

      PeerBlockBodyFetcher
        .makeActor(hostId, client, requestsProxy, transactionStore)
        .use { actor =>
          for {
            _ <- actor.send(PeerBlockBodyFetcher.Message.DownloadBlocks(idAndBody.map(_._1)))
          } yield ()
        }
    }
  }

  test("Block bodies shall be downloaded by request") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val transactionStore = mock[Store[F, Identifier.IoTransaction32, IoTransaction]]

      val (txs, bodies) =
        nonEmptyChainArbOf(TestHelper.arbitraryTxsAndBlock).arbitrary
          .retryUntil(c => c.size > 1 && c.size < maxChainSize)
          .first
          .unzip

      val blockIdsAndBodies =
        bodies.map(b => (co.topl.models.generators.consensus.ModelGenerators.arbitraryBlockId.arbitrary.first, b))

      val blockIds = blockIdsAndBodies.unzip._1

      val txIdsAndTxs = txs.toList.flatten.map(tx => (tx.id, tx))
      val (missedTxs, presentTxs) = txIdsAndTxs.partition { case (id, _) => id.hashCode() % 2 == 0 }

      val clientBodiesData = blockIdsAndBodies.toList.toMap
      (client.getRemoteBody _).expects(*).rep(blockIds.size.toInt).onCall { id: BlockId =>
        clientBodiesData.get(id).pure[F]
      }

      val transactionStoreData = presentTxs.toMap
      (transactionStore.contains _).expects(*).rep(txIdsAndTxs.size).onCall { id: Identifier.IoTransaction32 =>
        transactionStoreData.contains(id).pure[F]
      }

      val clientTxsData = missedTxs.toMap
      (client.getRemoteTransaction _).expects(*).rep(missedTxs.size).onCall { id: Identifier.IoTransaction32 =>
        clientTxsData.get(id).pure[F]
      }

      val downloadedTxs =
        mutable.Map.empty[Identifier.IoTransaction32, IoTransaction]
      (transactionStore.put _).expects(*, *).rep(missedTxs.size).onCall {
        case (id: Identifier.IoTransaction32, tx: IoTransaction) =>
          downloadedTxs.put(id, tx).pure[F].void
      }

      val wrappedBodies =
        blockIdsAndBodies.map { case (id, body) => (id, Either.right[BlockBodyDownloadError, BlockBody](body)) }
      val expectedMessage = RequestsProxy.Message.DownloadBlockResponse(hostId, wrappedBodies)
      (requestsProxy.sendNoWait _).expects(expectedMessage).once().returning(().pure[F])

      PeerBlockBodyFetcher
        .makeActor(hostId, client, requestsProxy, transactionStore)
        .use { actor =>
          for {
            _ <- actor.send(PeerBlockBodyFetcher.Message.DownloadBlocks(blockIds))
            _ = assert(downloadedTxs == missedTxs.toMap)
          } yield ()
        }
    }
  }

}
