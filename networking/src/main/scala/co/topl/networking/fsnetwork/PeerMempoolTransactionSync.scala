package co.topl.networking.fsnetwork

import cats.effect.kernel.Fiber
import cats.effect.{Async, Ref, Resource, Spawn}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.algebras.Store
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.models.p2p._
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockDownloadError.BlockBodyOrTransactionError
import co.topl.networking.fsnetwork.BlockDownloadError.BlockBodyOrTransactionError.UnknownError
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.P2PShowInstances._
import co.topl.typeclasses.implicits._
import fs2.Stream
import org.typelevel.log4cats.Logger
import cats.data.OptionT

object PeerMempoolTransactionSync {
  sealed trait Message

  object Message {
    case object StartActor extends Message
    case object StopActor extends Message
    case object CollectTransactionsRep extends Message
  }

  case class State[F[_]](
    hostId:             HostId,
    client:             BlockchainPeerClient[F],
    transactionStore:   Store[F, TransactionId, IoTransaction],
    transactionFetcher: TransactionFetcher[F],
    mempool:            MempoolAlgebra[F],
    peersManager:       PeersManagerActor[F],
    txsCount:           Ref[F, Long],
    localChain:         LocalChainAlgebra[F],
    fetchingFiber:      Option[Fiber[F, Throwable, Unit]]
  )

  type Response[F[_]] = State[F]
  type PeerMempoolTransactionSyncActor[F[_]] = Actor[F, Message, Response[F]]

  private val maxConcurrent = 16

  def getFsm[F[_]: Async: Logger]: Fsm[F, State[F], Message, Response[F]] = Fsm {
    case (state, Message.StartActor)             => startActor(state)
    case (state, Message.StopActor)              => stopActor(state)
    case (state, Message.CollectTransactionsRep) => collectTransactionsRep(state)
  }

  def makeActor[F[_]: Async: Logger](
    hostId:                      HostId,
    client:                      BlockchainPeerClient[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    mempool:                     MempoolAlgebra[F],
    peersManager:                PeersManagerActor[F],
    localChain:                  LocalChainAlgebra[F]
  ): Resource[F, PeerMempoolTransactionSyncActor[F]] = {
    val transactionFetcher = new TransactionFetcher[F](hostId, transactionSyntaxValidation, transactionStore, client)
    val txCount = Ref.unsafe(0L)
    val initialState =
      State(hostId, client, transactionStore, transactionFetcher, mempool, peersManager, txCount, localChain, None)
    val actorName = show"Mempool transaction sync for peer $hostId"
    Actor.makeWithFinalize(actorName, initialState, getFsm[F], finalizer[F])
  }

  private def finalizer[F[_]: Async: Logger](state: State[F]): F[Unit] =
    stopActor(state).void

  private def startActor[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    if (state.fetchingFiber.isEmpty) {
      for {
        _                 <- Logger[F].info(show"Start mempool sync actor for peer ${state.hostId}")
        transactionStream <- state.client.remoteTransactionNotifications
        fiber             <- Spawn[F].start(transactionFetcher(state, transactionStream).compile.drain)
        newState = state.copy(fetchingFiber = Option(fiber))
      } yield (newState, newState)
    } else {
      Logger[F].info(show"Ignore starting mempool sync actor for peer ${state.hostId}") >>
      (state, state).pure[F]
    }

  private def stopActor[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    state.fetchingFiber
      .map { fiber =>
        val newState = state.copy(fetchingFiber = None)

        Logger[F].info(show"Stop mempool sync fiber for peer ${state.hostId}") >>
        fiber.cancel >>
        (newState, newState).pure[F]
      }
      .getOrElse {
        Logger[F].info(show"Ignoring stopping mempool sync fetcher fiber for peer ${state.hostId}") >>
        (state, state).pure[F]
      }

  private def transactionFetcher[F[_]: Async: Logger](
    state:                State[F],
    transactionIdsStream: Stream[F, TransactionId]
  ): Stream[F, Unit] =
    transactionIdsStream
      .evalFilterAsync(maxConcurrent) { id =>
        {
          for {
            _    <- OptionT.whenM(state.transactionStore.contains(id).map(!_))(id.pure[F])
            head <- OptionT.liftF(state.localChain.head)
            _    <- OptionT.whenM(state.mempool.contains(head.slotId.blockId, id).map(!_))(id.pure[F])
          } yield id
        }.isDefined
      }
      .evalMap(processTransactionId(state))

  private def processTransactionId[F[_]: Async: Logger](state: State[F])(id: TransactionId): F[Unit] = {
    val fetchingTransaction =
      for {
        _ <- Logger[F].debug(show"Received mempool tx $id from ${state.hostId}")
        // tx download time could be used for performance measure,
        // but peers manager will be overwhelmed by messages
        (id, _) <- state.transactionFetcher.downloadCheckSaveTransaction(id, runSyntaxCheck = true)
        _       <- state.mempool.add(id)
        _       <- state.txsCount.update(_ + 1)
      } yield id

    fetchingTransaction
      .map(transactionId => Either.right[BlockBodyOrTransactionError, TransactionId](transactionId))
      .handleError {
        case error: BlockBodyOrTransactionError => Either.left[BlockBodyOrTransactionError, TransactionId](error)
        case unknownError                       => Either.left(UnknownError(unknownError))
      }
      .flatTap {
        case Right(txId) =>
          Logger[F].debug(show"Successfully sync transaction $txId to mempool from peer ${state.hostId}")
        case Left(error) =>
          Logger[F].error(show"Failed sync transaction from peer ${state.hostId} because of: ${error.toString}") >>
          error.notCritical
            .pure[F]
            .ifM(
              ifTrue = state.peersManager
                .sendNoWait(PeersManager.Message.NonCriticalErrorForHost(state.hostId)),
              ifFalse = state.peersManager.sendNoWait(PeersManager.Message.CriticalErrorForHost(state.hostId))
            )
      }
      .void
  }

  private def collectTransactionsRep[F[_]: Async](state: State[F]): F[(State[F], Response[F])] =
    state.txsCount.getAndSet(0).flatMap { txCount =>
      state.peersManager.sendNoWait(PeersManager.Message.ReceivedTransactionsCount(state.hostId, txCount))
    } >> (state, state).pure[F]
}
