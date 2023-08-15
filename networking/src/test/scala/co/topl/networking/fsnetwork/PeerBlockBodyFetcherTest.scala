package co.topl.networking.fsnetwork

import cats.data.NonEmptyChain
import cats.effect.{Async, IO}
import cats.implicits._
import co.topl.algebras.Store
import co.topl.brambl.generators.TransactionGenerator
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.algebras.BlockHeaderToBodyValidationAlgebra
import co.topl.consensus.models.BlockHeaderToBodyValidationFailure.IncorrectTxRoot
import co.topl.consensus.models.{BlockHeaderToBodyValidationFailure, BlockId}
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.TxRoot
import co.topl.models.generators.consensus.ModelGenerators
import co.topl.models.generators.consensus.ModelGenerators.nonEmptyChainArbOf
import co.topl.models.utility._
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockDownloadError.BlockBodyDownloadError
import co.topl.networking.fsnetwork.PeerBlockHeaderFetcherTest.F
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.networking.fsnetwork.TestHelper.{CallHandler1Ops, CallHandler2Ops}
import co.topl.networking.p2p.RemoteAddress
import co.topl.node.models.{Block, BlockBody}
import co.topl.typeclasses.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.function.FunctionAdapter1
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.mutable
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

object PeerBlockBodyFetcherTest {
  type F[A] = IO[A]
}

class PeerBlockBodyFetcherTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with TransactionGenerator {
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  val hostId: HostId = RemoteAddress("127.0.0.1", 0)
  val maxChainSize = 99

  private def compareWithoutDownloadTimeMatcher(
    rawExpectedMessage: RequestsProxy.Message
  ): FunctionAdapter1[RequestsProxy.Message, Boolean] = {
    val matchingFunction: RequestsProxy.Message => Boolean =
      (rawActualMessage: RequestsProxy.Message) =>
        (rawExpectedMessage, rawActualMessage) match {
          case (
                expectedMessage: RequestsProxy.Message.DownloadBodiesResponse,
                actualMessage: RequestsProxy.Message.DownloadBodiesResponse
              ) =>
            val newResp =
              actualMessage.response.map { case (header, res) =>
                (header, res.map(b => b.copy(downloadTimeMs = 0, downloadTimeTxMs = Seq.empty)))
              }
            expectedMessage == actualMessage.copy(response = newResp)
          case (_, _) => throw new IllegalStateException("Unexpected case")
        }
    new FunctionAdapter1[RequestsProxy.Message, Boolean](matchingFunction)
  }

  test("Block bodies shall return error if block is not present on client") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]

      val (txs, bodies) =
        nonEmptyChainArbOf(TestHelper.arbitraryTxsAndBlock).arbitrary
          .retryUntil(c => c.size > 1 && c.size < maxChainSize)
          .first
          .unzip

      val blockHeadersAndBodies =
        bodies.map(b => (ModelGenerators.arbitraryHeader.arbitrary.first.embedId, b))
      val blockIdsAndBodies = blockHeadersAndBodies.map { case (header, body) => (header.id, body) }

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
      (transactionStore.contains _).expects(*).anyNumberOfTimes().onCall { id: TransactionId =>
        transactionStoreData.contains(id).pure[F]
      }

      val clientTxsData = missedTxs.toMap
      (client.getRemoteTransaction _).expects(*).anyNumberOfTimes().onCall { id: TransactionId =>
        clientTxsData.get(id).pure[F]
      }

      val downloadedTxs =
        mutable.Map.empty[TransactionId, IoTransaction]
      (transactionStore.put _).expects(*, *).anyNumberOfTimes().onCall { case (id: TransactionId, tx: IoTransaction) =>
        downloadedTxs.put(id, tx).pure[F].void
      }

      (headerToBodyValidation.validate _).expects(*).rep(presentBlockIdAndBodies.size).onCall { block: Block =>
        Either.right[BlockHeaderToBodyValidationFailure, Block](block).pure[F]
      }

      val wrappedBodies =
        blockHeadersAndBodies.map { case (header, body) =>
          if (clientBodiesData.contains(header.id)) {
            (header, Either.right[BlockBodyDownloadError, UnverifiedBlockBody](UnverifiedBlockBody(hostId, body, 0)))
          } else {
            (
              header,
              Either.left[BlockBodyDownloadError, UnverifiedBlockBody](BlockBodyDownloadError.BodyNotFoundInPeer)
            )
          }
        }

      val expectedMessage: RequestsProxy.Message = RequestsProxy.Message.DownloadBodiesResponse(hostId, wrappedBodies)
      (requestsProxy.sendNoWait _)
        .expects(compareWithoutDownloadTimeMatcher(expectedMessage))
        .once()
        .returning(().pure[F])

      PeerBlockBodyFetcher
        .makeActor(hostId, client, requestsProxy, transactionStore, headerToBodyValidation)
        .use { actor =>
          for {
            _ <- actor.send(PeerBlockBodyFetcher.Message.DownloadBlocks(blockHeadersAndBodies.map(_._1)))
            _ = assert(downloadedTxs.size <= missedTxs.toMap.size)
          } yield ()
        }
    }
  }

  test("Block bodies shall return error if block have incorrect txRoot") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]

      val (txs, bodies) =
        nonEmptyChainArbOf(TestHelper.arbitraryTxsAndBlock).arbitrary
          .retryUntil(c => c.size > 1 && c.size < maxChainSize)
          .first
          .unzip

      val blockIdsBodiesHeaders =
        bodies.map { body: BlockBody =>
          val header =
            ModelGenerators.arbitraryHeader.arbitrary.first.copy(txRoot = body.merkleTreeRootHash.data).embedId
          val id = header.id
          (id, body, header)
        }
      val blockIdsAndBodies = blockIdsBodiesHeaders.map(d => (d._1, d._2))

      val (correctTxRootBlockIds, _) = blockIdsAndBodies.toList.map(_._1).partition(_.hashCode() % 2 == 0)

      val txIdsAndTxs = txs.toList.flatten.map(tx => (tx.id, tx))
      val (missedTxs, presentTxs) = txIdsAndTxs.partition { case (id, _) => id.hashCode() % 2 == 0 }

      val clientBodiesData = blockIdsAndBodies.toList.toMap
      (client.getRemoteBody _).expects(*).rep(blockIdsAndBodies.size.toInt).onCall { id: BlockId =>
        clientBodiesData.get(id).pure[F]
      }

      val transactionStoreData = presentTxs.toMap
      (transactionStore.contains _).expects(*).anyNumberOfTimes().onCall { id: TransactionId =>
        transactionStoreData.contains(id).pure[F]
      }

      val clientTxsData = missedTxs.toMap
      (client.getRemoteTransaction _).expects(*).anyNumberOfTimes().onCall { id: TransactionId =>
        clientTxsData.get(id).pure[F]
      }

      val downloadedTxs =
        mutable.Map.empty[TransactionId, IoTransaction]
      (transactionStore.put _).expects(*, *).anyNumberOfTimes().onCall { case (id: TransactionId, tx: IoTransaction) =>
        downloadedTxs.put(id, tx).pure[F].void
      }

      val incorrectTxRoot: TxRoot = ModelGenerators.txRoot.first
      (headerToBodyValidation.validate _).expects(*).rep(blockIdsAndBodies.size.toInt).onCall { block: Block =>
        if (correctTxRootBlockIds.contains(block.header.id)) {
          Either.right[BlockHeaderToBodyValidationFailure, Block](block).pure[F]
        } else {
          Either
            .left[BlockHeaderToBodyValidationFailure, Block](
              IncorrectTxRoot(block.body.merkleTreeRootHash, incorrectTxRoot)
            )
            .pure[F]
        }
      }

      val wrappedBodies =
        blockIdsBodiesHeaders.map { case (id, body, header) =>
          if (correctTxRootBlockIds.contains(id)) {
            (header, Either.right[BlockBodyDownloadError, UnverifiedBlockBody](UnverifiedBlockBody(hostId, body, 0)))
          } else {
            (
              header,
              Either.left[BlockBodyDownloadError, UnverifiedBlockBody](
                BlockBodyDownloadError.BodyHaveIncorrectTxRoot(body.merkleTreeRootHash, incorrectTxRoot)
              )
            )
          }
        }

      val expectedMessage = RequestsProxy.Message.DownloadBodiesResponse(hostId, wrappedBodies)
      (requestsProxy.sendNoWait _)
        .expects(compareWithoutDownloadTimeMatcher(expectedMessage))
        .once()
        .returning(().pure[F])

      val sendMessage = blockIdsBodiesHeaders.map(_._3)
      PeerBlockBodyFetcher
        .makeActor(hostId, client, requestsProxy, transactionStore, headerToBodyValidation)
        .use { actor =>
          for {
            _ <- actor.send(PeerBlockBodyFetcher.Message.DownloadBlocks(sendMessage))
            _ = assert(downloadedTxs.size <= missedTxs.toMap.size)
          } yield ()
        }
    }
  }

  test("Block bodies shall return error if client has no transaction") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]

      val transactionsAndBody =
        nonEmptyChainArbOf(TestHelper.arbitraryTxsAndBlock).arbitrary
          .retryUntil(c => c.size > 1 && c.size < maxChainSize)
          .first

      val headerBodyTxIdTx =
        transactionsAndBody
          .map { case (txs, body) =>
            val header =
              ModelGenerators.arbitraryHeader.arbitrary.first.embedId.copy(txRoot = body.merkleTreeRootHash.data)

            (header, body, txs.map(tx => (tx.id, tx)))
          }

      val idBodyTxIdTx = headerBodyTxIdTx.map(d => (d._1.id, d._2, d._3))
      val idAndBody = idBodyTxIdTx.map(d => (d._1, d._2))

      def transactionIsMissed(id:  TransactionId): Boolean = id.hashCode() % 7 == 0
      def blockIsMissed(blockBody: BlockBody): Boolean = blockBody.allTransactionIds.exists(transactionIsMissed)

      val presentBlockIdAndBodies = idAndBody.toList

      val txIdsAndTxs =
        idBodyTxIdTx.map(d => d._3).toList.flatten

      val clientBodiesData = presentBlockIdAndBodies.toMap
      (client.getRemoteBody _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        clientBodiesData.get(id).pure[F]
      }

      (transactionStore.contains _).expects(*).anyNumberOfTimes().returning(false.pure[F])

      val clientTxsData = txIdsAndTxs.toMap
      (client.getRemoteTransaction _).expects(*).anyNumberOfTimes().onCall { id: TransactionId =>
        if (transactionIsMissed(id)) {
          Option.empty[IoTransaction].pure[F]
        } else {
          clientTxsData.get(id).pure[F]
        }
      }

      (transactionStore.put _).expects(*, *).anyNumberOfTimes().returning(().pure[F])

      (headerToBodyValidation.validate _).expects(*).rep(idAndBody.size.toInt).onCall { block: Block =>
        Either.right[BlockHeaderToBodyValidationFailure, Block](block).pure[F]
      }

      val wrappedBodies =
        headerBodyTxIdTx.map { case (header, body, _) =>
          if (!blockIsMissed(body)) {
            (header, Either.right[BlockBodyDownloadError, UnverifiedBlockBody](UnverifiedBlockBody(hostId, body, 0)))
          } else {
            val missedId = body.allTransactionIds.find(transactionIsMissed).get
            (
              header,
              Either.left[BlockBodyDownloadError, UnverifiedBlockBody](
                BlockBodyDownloadError.TransactionNotFoundInPeer(missedId)
              )
            )
          }
        }

      val expectedMessage =
        RequestsProxy.Message.DownloadBodiesResponse(hostId, wrappedBodies)
      (requestsProxy.sendNoWait _)
        .expects(compareWithoutDownloadTimeMatcher(expectedMessage))
        .once()
        .returning(().pure[F])

      PeerBlockBodyFetcher
        .makeActor(hostId, client, requestsProxy, transactionStore, headerToBodyValidation)
        .use { actor =>
          for {
            _ <- actor.send(PeerBlockBodyFetcher.Message.DownloadBlocks(headerBodyTxIdTx.map(_._1)))
          } yield ()
        }
    }
  }

  test("Block bodies shall return error if client has transaction with incorrect id") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]

      val transactionsAndBody =
        nonEmptyChainArbOf(TestHelper.arbitraryTxsAndBlock).arbitrary
          .retryUntil(c => c.size > 1 && c.size < maxChainSize)
          .first

      val headerBodyTxIdTx =
        transactionsAndBody
          .map { case (txs, body) =>
            val header =
              ModelGenerators.arbitraryHeader.arbitrary.first.embedId.copy(txRoot = body.merkleTreeRootHash.data)

            (header, body, txs.map(tx => (tx.id, tx)))
          }

      val idBodyTxIdTx = headerBodyTxIdTx.map(d => (d._1.id, d._2, d._3))

      def transactionHaveIncorrectId(id: TransactionId): Boolean = id.hashCode() % 7 == 0

      def blockIsMissed(blockBody: BlockBody): Boolean = blockBody.allTransactionIds.exists(transactionHaveIncorrectId)

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
      (client.getRemoteTransaction _).expects(*).anyNumberOfTimes().onCall { id: TransactionId =>
        if (transactionHaveIncorrectId(id)) {
          Option(incorrectTransaction).pure[F]
        } else {
          clientTxsData.get(id).pure[F]
        }
      }

      (transactionStore.put _).expects(*, *).anyNumberOfTimes().returning(().pure[F])

      (headerToBodyValidation.validate _).expects(*).rep(idAndBody.size.toInt).onCall { block: Block =>
        Either.right[BlockHeaderToBodyValidationFailure, Block](block).pure[F]
      }

      val wrappedBodies =
        headerBodyTxIdTx.map { case (header, body, _) =>
          if (!blockIsMissed(body)) {
            (header, Either.right[BlockBodyDownloadError, UnverifiedBlockBody](UnverifiedBlockBody(hostId, body, 0)))
          } else {
            val expectedId = body.allTransactionIds.find(transactionHaveIncorrectId).get
            (
              header,
              Either.left[BlockBodyDownloadError, UnverifiedBlockBody](
                BlockBodyDownloadError.TransactionHaveIncorrectId(expectedId, incorrectTransactionId)
              )
            )
          }
        }

      val expectedMessage =
        RequestsProxy.Message.DownloadBodiesResponse(hostId, wrappedBodies)
      (requestsProxy.sendNoWait _)
        .expects(compareWithoutDownloadTimeMatcher(expectedMessage))
        .once()
        .returning(().pure[F])

      PeerBlockBodyFetcher
        .makeActor(hostId, client, requestsProxy, transactionStore, headerToBodyValidation)
        .use { actor =>
          for {
            _ <- actor.send(PeerBlockBodyFetcher.Message.DownloadBlocks(headerBodyTxIdTx.map(_._1)))
          } yield ()
        }
    }
  }

  test("Block bodies shall be downloaded by request") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]

      val (txs, bodies) =
        nonEmptyChainArbOf(TestHelper.arbitraryTxsAndBlock).arbitrary
          .retryUntil(c => c.size > 1 && c.size < maxChainSize)
          .first
          .unzip

      val headerBody =
        bodies
          .map { body =>
            val header =
              ModelGenerators.arbitraryHeader.arbitrary.first.embedId.copy(txRoot = body.merkleTreeRootHash.data)
            (header, body)
          }
      val blockIdsAndBodies = headerBody.map { case (header, body) => (header.id, body) }

      val blockIds = blockIdsAndBodies.unzip._1

      val txIdsAndTxs = txs.toList.flatten.map(tx => (tx.id, tx))
      val (missedTxs, presentTxs) = txIdsAndTxs.partition { case (id, _) => id.hashCode() % 2 == 0 }

      val clientBodiesData = blockIdsAndBodies.toList.toMap
      (client.getRemoteBody _).expects(*).rep(blockIds.size.toInt).onCall { id: BlockId =>
        clientBodiesData.get(id).pure[F]
      }

      val transactionStoreData = presentTxs.toMap
      (transactionStore.contains _).expects(*).rep(txIdsAndTxs.size).onCall { id: TransactionId =>
        transactionStoreData.contains(id).pure[F]
      }

      val clientTxsData = missedTxs.toMap
      (client.getRemoteTransaction _).expects(*).rep(missedTxs.size).onCall { id: TransactionId =>
        clientTxsData.get(id).pure[F]
      }

      val downloadedTxs =
        mutable.Map.empty[TransactionId, IoTransaction]
      (transactionStore.put _).expects(*, *).rep(missedTxs.size).onCall { case (id: TransactionId, tx: IoTransaction) =>
        downloadedTxs.put(id, tx).pure[F].void
      }

      (headerToBodyValidation.validate _).expects(*).rep(blockIdsAndBodies.size.toInt).onCall { block: Block =>
        Either.right[BlockHeaderToBodyValidationFailure, Block](block).pure[F]
      }

      val wrappedBodies =
        headerBody.map { case (header, body) =>
          (header, Either.right[BlockBodyDownloadError, UnverifiedBlockBody](UnverifiedBlockBody(hostId, body, 0)))
        }
      val expectedMessage = RequestsProxy.Message.DownloadBodiesResponse(hostId, wrappedBodies)
      (requestsProxy.sendNoWait _)
        .expects(compareWithoutDownloadTimeMatcher(expectedMessage))
        .once()
        .returning(().pure[F])

      PeerBlockBodyFetcher
        .makeActor(hostId, client, requestsProxy, transactionStore, headerToBodyValidation)
        .use { actor =>
          for {
            _ <- actor.send(PeerBlockBodyFetcher.Message.DownloadBlocks(headerBody.map(_._1)))
            _ = assert(downloadedTxs == missedTxs.toMap)
          } yield ()
        }
    }
  }

  test("Block bodies and transactions shall have proper download time") {
    withMock {
      val bodyDelay = 90
      val txDelay = 10
      val client = mock[BlockchainPeerClient[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]

      val (txs, body) =
        TestHelper.arbitraryTxsAndBlock.arbitrary.first

      val header =
        ModelGenerators.arbitraryHeader.arbitrary.first.embedId.copy(txRoot = body.merkleTreeRootHash.data)

      val txIdsAndTxs = txs.map(tx => (tx.id, tx)).toMap

      (client.getRemoteBody _)
        .expects(header.id)
        .once()
        .returns(
          Async[F].delayBy(Option(body).pure[F], FiniteDuration(bodyDelay, MILLISECONDS))
        )

      (transactionStore.contains _).expects(*).once().returns(true.pure[F])
      (transactionStore.contains _).expects(*).rep(txIdsAndTxs.size - 1).returns(false.pure[F])

      (client.getRemoteTransaction _).expects(*).rep(txIdsAndTxs.size - 1).onCall { id: TransactionId =>
        Async[F].delayBy(txIdsAndTxs.get(id).pure[F], FiniteDuration(txDelay, MILLISECONDS))
      }

      val downloadedTxs =
        mutable.Map.empty[TransactionId, IoTransaction]
      (transactionStore.put _).expects(*, *).rep(txIdsAndTxs.size - 1).onCall {
        case (id: TransactionId, tx: IoTransaction) =>
          downloadedTxs.put(id, tx).pure[F].void
      }

      (headerToBodyValidation.validate _).expects(*).once().onCall { block: Block =>
        Either.right[BlockHeaderToBodyValidationFailure, Block](block).pure[F]
      }

      (requestsProxy.sendNoWait _).expects(*).once().onCall { message: RequestsProxy.Message =>
        message match {
          case RequestsProxy.Message.DownloadBodiesResponse(`hostId`, bodies) =>
            if (
              bodies.forall { case (_, resp) =>
                resp.forall(body =>
                  (body.downloadTimeMs >= bodyDelay) &&
                  (body.downloadTimeMs < bodyDelay + 30) &&
                  body.downloadTimeTxMs.size == txIdsAndTxs.size - 1 &&
                  body.downloadTimeTxMs.forall(tx => tx >= txDelay && tx < txDelay + 10)
                )
              }
            )
              ().pure[F]
            else
              throw new IllegalStateException()
          case _ => throw new IllegalStateException()
        }
      }

      PeerBlockBodyFetcher
        .makeActor(hostId, client, requestsProxy, transactionStore, headerToBodyValidation)
        .use { actor =>
          for {
            _ <- actor.send(PeerBlockBodyFetcher.Message.DownloadBlocks(NonEmptyChain.one(header)))
          } yield ()
        }
    }
  }

}
