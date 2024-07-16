package co.topl.networking.fsnetwork

import cats.data.NonEmptyChain
import cats.{Applicative, MonadThrow}
import cats.effect.IO
import cats.effect.Sync
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import cats.implicits._
import co.topl.algebras.Store
import co.topl.brambl.generators.ModelGenerators.arbitraryIoTransaction
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.brambl.validation.TransactionSyntaxError
import co.topl.brambl.validation.TransactionSyntaxError.EmptyInputs
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.consensus.models.SlotData
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models.p2p._
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockDownloadError.BlockBodyOrTransactionError
import co.topl.networking.fsnetwork.PeerMempoolTransactionSyncTest.F
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.TestHelper.{arbitraryHost, BlockBodyOrTransactionErrorByName}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import fs2.Stream

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration._

object PeerMempoolTransactionSyncTest {
  type F[A] = IO[A]
}

sealed trait TransactionType

object TransactionType {

  case object TransactionIsStore
  case object MissedTransactionCorrect
  case object MissedTransactionBadId
  case object MissedTransactionBadSyntax
}

class PeerMempoolTransactionSyncTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  val hostId: HostId = arbitraryHost.arbitrary.first
  val headBlock: SlotData = arbitrarySlotData.arbitrary.first

  test("Transaction notification shall be started and missed transaction shall be requested") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]
      (mempool.contains _).expects(headBlock.slotId.blockId, *).anyNumberOfTimes().returns(false.pure[F])
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val peersManager = mock[PeersManagerActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.head).expects().anyNumberOfTimes().returns(headBlock.pure[F])

      val totalTransactions = 10
      val transactions = Seq.fill(totalTransactions)(arbitraryIoTransaction.arbitrary.first).map(_.embedId)
      val (missedTransaction, transactionInStore) = transactions.splitAt(totalTransactions / 2 + 1)

      val txSentFlag: AtomicBoolean = new AtomicBoolean(false)
      (() => client.remoteTransactionNotifications).expects().once().onCall { () =>
        Stream.emits(transactions.map(_.id)).covary[F].pure[F] <* Sync[F].delay(txSentFlag.set(true))
      }

      val missedMap = missedTransaction.map(tx => tx.id -> tx).toMap
      missedTransaction.map { tx =>
        (transactionStore.contains _).expects(tx.id).once().returns(false.pure[F])
        (client
          .getRemoteTransactionOrError(_: TransactionId, _: BlockBodyOrTransactionError)(_: MonadThrow[F]))
          .expects(*, *, *)
          .once()
          .onCall {
            case (id: TransactionId, _: BlockBodyOrTransactionErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
              missedMap(id).pure[F]
          }
        (transactionSyntaxValidation.validate _).expects(tx).once().returns(Either.right(tx).pure[F])
        (transactionStore.put _).expects(tx.id, tx).once().returns(Applicative[F].unit)
        (mempool.add _).expects(tx.id).once().returns(true.pure[F])
      }

      transactionInStore.map { tx =>
        (transactionStore.contains _).expects(tx.id).once().returns(true.pure[F])
      }

      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.ReceivedTransactionsCount(hostId, missedTransaction.size))
        .returns(Applicative[F].unit)

      val timeout = FiniteDuration(1000, MILLISECONDS)

      PeerMempoolTransactionSync
        .makeActor(hostId, client, transactionSyntaxValidation, transactionStore, mempool, peersManager, localChain)
        .use { actor =>
          for {
            state <- actor.send(PeerMempoolTransactionSync.Message.StartActor)
            _     <- Sync[F].untilM_(Sync[F].sleep(timeout))(Sync[F].delay(txSentFlag.get()))
            state <- actor.send(PeerMempoolTransactionSync.Message.CollectTransactionsRep)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("Transaction notification shall be started and missed transaction shall be requested") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val peersManager = mock[PeersManagerActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.head).expects().anyNumberOfTimes().returns(headBlock.pure[F])

      val totalTransactions = 10
      val transactions = Seq.fill(totalTransactions)(arbitraryIoTransaction.arbitrary.first).map(_.embedId)
      val (missedTransaction, transactionInMempool) = transactions.splitAt(totalTransactions / 2 + 1)

      val txSentFlag: AtomicBoolean = new AtomicBoolean(false)
      (() => client.remoteTransactionNotifications).expects().once().onCall { () =>
        Stream.emits(transactions.map(_.id)).covary[F].pure[F] <* Sync[F].delay(txSentFlag.set(true))
      }

      val missedMap = missedTransaction.map(tx => tx.id -> tx).toMap
      missedTransaction.map { tx =>
        (transactionStore.contains _).expects(tx.id).once().returns(false.pure[F])
        (client
          .getRemoteTransactionOrError(_: TransactionId, _: BlockBodyOrTransactionError)(_: MonadThrow[F]))
          .expects(*, *, *)
          .once()
          .onCall {
            case (id: TransactionId, _: BlockBodyOrTransactionErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
              missedMap(id).pure[F]
          }
        (transactionSyntaxValidation.validate _).expects(tx).once().returns(Either.right(tx).pure[F])
        (transactionStore.put _).expects(tx.id, tx).once().returns(Applicative[F].unit)
        (mempool.add _).expects(tx.id).once().returns(true.pure[F])
      }

      (transactionStore.contains _).expects(*).anyNumberOfTimes().returns(false.pure[F])
      transactionInMempool.map { tx =>
        (mempool.contains _).expects(headBlock.slotId.blockId, tx.id).once().returns(true.pure[F])
      }
      missedTransaction.map { tx =>
        (mempool.contains _).expects(headBlock.slotId.blockId, tx.id).once().returns(false.pure[F])
      }

      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.ReceivedTransactionsCount(hostId, missedTransaction.size))
        .returns(Applicative[F].unit)

      val timeout = FiniteDuration(1000, MILLISECONDS)

      PeerMempoolTransactionSync
        .makeActor(hostId, client, transactionSyntaxValidation, transactionStore, mempool, peersManager, localChain)
        .use { actor =>
          for {
            state <- actor.send(PeerMempoolTransactionSync.Message.StartActor)
            _     <- Sync[F].untilM_(Sync[F].sleep(timeout))(Sync[F].delay(txSentFlag.get()))
            state <- actor.send(PeerMempoolTransactionSync.Message.CollectTransactionsRep)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("Transaction notification shall sent appropriate message if transaction is syntactically incorrect") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val peersManager = mock[PeersManagerActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.head).expects().anyNumberOfTimes().returns(headBlock.pure[F])
      (mempool.contains _).expects(headBlock.slotId.blockId, *).anyNumberOfTimes().returns(false.pure[F])

      val totalTransactions = 1
      val transactions = Seq.fill(totalTransactions)(arbitraryIoTransaction.arbitrary.first).map(_.embedId)

      (() => client.remoteTransactionNotifications).expects().once().onCall { () =>
        Stream.emits(transactions.map(_.id)).covary[F].pure[F]
      }

      val missedMap = transactions.map(tx => tx.id -> tx).toMap
      transactions.map { tx =>
        (transactionStore.contains _).expects(tx.id).once().returns(false.pure[F])
        (client
          .getRemoteTransactionOrError(_: TransactionId, _: BlockBodyOrTransactionError)(_: MonadThrow[F]))
          .expects(*, *, *)
          .once()
          .onCall {
            case (id: TransactionId, _: BlockBodyOrTransactionErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
              missedMap(id).pure[F]
          }
        (transactionSyntaxValidation.validate _)
          .expects(tx)
          .returns(
            Either.left[NonEmptyChain[TransactionSyntaxError], IoTransaction](NonEmptyChain.one(EmptyInputs)).pure[F]
          )
        (peersManager.sendNoWait _)
          .expects(PeersManager.Message.CriticalErrorForHost(hostId))
          .once()
          .returns(Applicative[F].unit)
        (transactionStore.put _).expects(tx.id, tx).never().returns(Applicative[F].unit)
        (mempool.add _).expects(tx.id).never().returns(true.pure[F])
      }

      PeerMempoolTransactionSync
        .makeActor(hostId, client, transactionSyntaxValidation, transactionStore, mempool, peersManager, localChain)
        .use { actor =>
          for {
            state <- actor.send(PeerMempoolTransactionSync.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("Transaction notification shall sent appropriate message if transaction have incorrect id") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val peersManager = mock[PeersManagerActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.head).expects().anyNumberOfTimes().returns(headBlock.pure[F])
      (mempool.contains _).expects(headBlock.slotId.blockId, *).anyNumberOfTimes().returns(false.pure[F])

      val totalTransactions = 1
      val transactions = Seq.fill(totalTransactions)(arbitraryIoTransaction.arbitrary.first).map(_.embedId)

      (() => client.remoteTransactionNotifications).expects().once().onCall { () =>
        Stream.emits(transactions.map(_.id)).covary[F].pure[F]
      }

      transactions.map { tx =>
        (transactionStore.contains _).expects(tx.id).once().returns(false.pure[F])
        (client
          .getRemoteTransactionOrError(_: TransactionId, _: BlockBodyOrTransactionError)(_: MonadThrow[F]))
          .expects(*, *, *)
          .once()
          .onCall {
            case (_: TransactionId, _: BlockBodyOrTransactionErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
              arbitraryIoTransaction.arbitrary.first.pure[F]
          }
        (peersManager.sendNoWait _)
          .expects(PeersManager.Message.CriticalErrorForHost(hostId))
          .once()
          .returns(Applicative[F].unit)
        (transactionSyntaxValidation.validate _).expects(tx).never().returns(Either.right(tx).pure[F])
        (transactionStore.put _).expects(tx.id, tx).never().returns(Applicative[F].unit)
        (mempool.add _).expects(tx.id).never().returns(true.pure[F])
      }

      PeerMempoolTransactionSync
        .makeActor(hostId, client, transactionSyntaxValidation, transactionStore, mempool, peersManager, localChain)
        .use { actor =>
          for {
            state <- actor.send(PeerMempoolTransactionSync.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("Transaction notification shall be started in case of unknown error message is sent") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val peersManager = mock[PeersManagerActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.head).expects().anyNumberOfTimes().returns(headBlock.pure[F])
      (mempool.contains _).expects(headBlock.slotId.blockId, *).anyNumberOfTimes().returns(false.pure[F])

      val totalTransactions = 10
      val transactions = Seq.fill(totalTransactions)(arbitraryIoTransaction.arbitrary.first).map(_.embedId)
      val (missedTransaction, transactionInStore) = transactions.splitAt(totalTransactions / 2)

      (() => client.remoteTransactionNotifications).expects().once().onCall { () =>
        Stream.emits(transactions.map(_.id)).covary[F].pure[F]
      }

      missedTransaction.map { tx =>
        (transactionStore.contains _).expects(tx.id).once().returns(false.pure[F])
        (client
          .getRemoteTransactionOrError(_: TransactionId, _: BlockBodyOrTransactionError)(_: MonadThrow[F]))
          .expects(*, *, *)
          .once()
          .onCall {
            case (_: TransactionId, _: BlockBodyOrTransactionErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
              throw new RuntimeException()
          }
        (peersManager.sendNoWait _)
          .expects(PeersManager.Message.NonCriticalErrorForHost(hostId))
          .once()
          .returns(Applicative[F].unit)
        (transactionSyntaxValidation.validate _).expects(tx).never().returns(Either.right(tx).pure[F])
        (transactionStore.put _).expects(tx.id, tx).never().returns(Applicative[F].unit)
        (mempool.add _).expects(tx.id).never().returns(true.pure[F])
      }

      transactionInStore.map { tx =>
        (transactionStore.contains _).expects(tx.id).once().returns(true.pure[F])
      }

      PeerMempoolTransactionSync
        .makeActor(hostId, client, transactionSyntaxValidation, transactionStore, mempool, peersManager, localChain)
        .use { actor =>
          for {
            state <- actor.send(PeerMempoolTransactionSync.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("Double start shall not re-create fiber") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val peersManager = mock[PeersManagerActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.head).expects().anyNumberOfTimes().returns(headBlock.pure[F])
      (mempool.contains _).expects(headBlock.slotId.blockId, *).anyNumberOfTimes().returns(false.pure[F])

      (() => client.remoteTransactionNotifications).expects().once().onCall { () =>
        Stream.fromOption[F](Option.empty[TransactionId]).pure[F]
      }

      PeerMempoolTransactionSync
        .makeActor(hostId, client, transactionSyntaxValidation, transactionStore, mempool, peersManager, localChain)
        .use { actor =>
          for {
            state1 <- actor.send(PeerMempoolTransactionSync.Message.StartActor)
            fiber1 = state1.fetchingFiber
            state2 <- actor.send(PeerMempoolTransactionSync.Message.StartActor)
            fiber2 = state2.fetchingFiber
            _ = assert(fiber1 == fiber2)
            _ <- state2.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("Double stop shall not stop fiber twice") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val peersManager = mock[PeersManagerActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.head).expects().anyNumberOfTimes().returns(headBlock.pure[F])
      (mempool.contains _).expects(headBlock.slotId.blockId, *).anyNumberOfTimes().returns(false.pure[F])

      (() => client.remoteTransactionNotifications).expects().once().onCall { () =>
        Stream.fromOption[F](Option.empty[TransactionId]).pure[F]
      }

      PeerMempoolTransactionSync
        .makeActor(hostId, client, transactionSyntaxValidation, transactionStore, mempool, peersManager, localChain)
        .use { actor =>
          for {
            state1 <- actor.send(PeerMempoolTransactionSync.Message.StartActor)
            _ = assert(state1.fetchingFiber.isDefined)
            state2 <- actor.send(PeerMempoolTransactionSync.Message.StopActor)
            _ = assert(state2.fetchingFiber.isEmpty)
            state3 <- actor.send(PeerMempoolTransactionSync.Message.StopActor)
            _ = assert(state3.fetchingFiber.isEmpty)
          } yield ()
        }
    }
  }

}
