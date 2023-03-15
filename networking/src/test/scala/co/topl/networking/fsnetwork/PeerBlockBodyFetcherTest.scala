package co.topl.networking.fsnetwork

import cats.effect.IO
import cats.implicits._
import co.topl.algebras.Store
import co.topl.brambl.generators.TransactionGenerator
import co.topl.brambl.models.Identifier
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances.ioTransactionAsIoTransactionOps
import co.topl.consensus.models.BlockId
import co.topl.models.generators.consensus.ModelGenerators.nonEmptyChainArbOf
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.networking.fsnetwork.PeerBlockHeaderFetcherTest.F
import co.topl.networking.fsnetwork.TestHelper.{CallHandler1Ops, CallHandler2Ops}
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

  test("Block bodies shall be downloaded by request") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val transactionStore = mock[Store[F, Identifier.IoTransaction32, IoTransaction]]

      val (txs, bodies) =
        nonEmptyChainArbOf(TestHelper.arbitraryTxsAndBlock).arbitrary
          .retryUntil(c => c.size > 1 && c.size < maxChainSize)
          .sample
          .get
          .unzip

      val blockIdsAndBodies =
        bodies.map(b => (co.topl.models.generators.consensus.ModelGenerators.arbitraryBlockId.arbitrary.sample.get, b))

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

      val expectedMessage = BlockChecker.Message.RemoteBlockBodies(hostId, blockIdsAndBodies)
      (blockChecker.sendNoWait _).expects(expectedMessage).once().onCall { _: BlockChecker.Message => ().pure[F] }

      for {
        (actor, shutdown) <-
          PeerBlockBodyFetcher
            .makeActor(hostId, client, blockChecker, transactionStore)
            .allocated
        _ <- actor.send(PeerBlockBodyFetcher.Message.DownloadBlocks(blockIds))
        _ = assert(downloadedTxs == missedTxs.toMap)
        shutdownFiber <- actor.gracefulShutdown(shutdown)
        _             <- shutdownFiber.join
      } yield ()
    }
  }

}
