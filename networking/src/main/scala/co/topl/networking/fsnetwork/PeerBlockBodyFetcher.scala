package co.topl.networking.fsnetwork

import cats.MonadThrow
import cats.data.{NonEmptyChain, OptionT}
import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.algebras.Store
import co.topl.brambl.models.Identifier
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.models.BlockId
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockBodyDownloadError._
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.node.models.BlockBody
import co.topl.typeclasses.implicits._
import fs2.Stream
import org.typelevel.log4cats.Logger

object PeerBlockBodyFetcher {
  sealed trait Message

  object Message {
    case object StopActor extends Message

    case object StartActor extends Message

    /**
     * Request to download block bodies from peer, downloaded bodies will be sent to block checker directly
     *
     * @param blockIds bodies block id to download
     */
    case class DownloadBlocks(blockIds: NonEmptyChain[BlockId]) extends Message

  }

  case class State[F[_]](
    hostId:           HostId,
    client:           BlockchainPeerClient[F],
    requestsProxy:    RequestsProxyActor[F],
    transactionStore: Store[F, Identifier.IoTransaction32, IoTransaction]
  )

  type Response[F[_]] = State[F]
  type PeerBlockBodyFetcherActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger]: Fsm[F, State[F], Message, Response[F]] = Fsm {
    case (state, Message.DownloadBlocks(blocksToDownload)) => downloadBodies(state, blocksToDownload)
    case (state, Message.StartActor)                       => startActor(state)
    case (state, Message.StopActor)                        => stopActor(state)
  }

  def makeActor[F[_]: Async: Logger](
    hostId:           HostId,
    client:           BlockchainPeerClient[F],
    requestsProxy:    RequestsProxyActor[F],
    transactionStore: Store[F, Identifier.IoTransaction32, IoTransaction]
  ): Resource[F, PeerBlockBodyFetcherActor[F]] = {
    val initialState = State(hostId, client, requestsProxy, transactionStore)
    Actor.make(initialState, getFsm[F])
  }

  private def downloadBodies[F[_]: Async: Logger](
    state:            State[F],
    blocksToDownload: NonEmptyChain[BlockId]
  ): F[(State[F], Response[F])] =
    for {
      idToBody <- Stream.foldable(blocksToDownload).evalMap(downloadBlockBody(state)).compile.toList
      messageToSend = RequestsProxy.Message.DownloadBlockResponse(state.hostId, NonEmptyChain.fromSeq(idToBody).get)
      _ <- state.requestsProxy.sendNoWait(messageToSend)
    } yield (state, state)

  private def downloadBlockBody[F[_]: Async: Logger](
    state: State[F]
  )(blockId: BlockId): F[(BlockId, Either[BlockBodyDownloadError, BlockBody])] = {
    val bodyEither: F[BlockBody] =
      for {
        _    <- Logger[F].info(show"Fetching remote body id=$blockId")
        body <- downloadBlockBody(state, blockId)
        _    <- Logger[F].debug(show"Fetched remote body id=$blockId")
        _    <- downloadingMissingTransactions(state, body)
      } yield body

    bodyEither
      .map(blockBody => Either.right[BlockBodyDownloadError, BlockBody](blockBody))
      .handleError {
        case e: BlockBodyDownloadError => Either.left[BlockBodyDownloadError, BlockBody](e)
        case unknownError              => Either.left(UnknownError(unknownError))
      }
      .flatTap {
        case Right(_) =>
          Logger[F].debug(show"Successfully download block $blockId from peer ${state.hostId}")
        case Left(error) =>
          Logger[F].error(show"Failed download block $blockId from peer ${state.hostId} because of: ${error.toString}")
      }
      .map((blockId, _))

  }

  private def downloadBlockBody[F[_]: Async](
    state:   State[F],
    blockId: BlockId
  ): F[BlockBody] =
    OptionT(state.client.getRemoteBody(blockId)).getOrElseF(MonadThrow[F].raiseError(BodyNotFoundInPeer))

  private def downloadingMissingTransactions[F[_]: Async: Logger](
    state:     State[F],
    blockBody: BlockBody
  ): F[List[Identifier.IoTransaction32]] =
    Stream
      .iterable[F, Identifier.IoTransaction32](blockBody.transactionIds)
      .evalMap(transactionId =>
        state.transactionStore
          .contains(transactionId)
          .flatMap {
            case true  => transactionId.pure[F]
            case false => downloadAndCheckTransaction(state, transactionId)
          }
      )
      .compile
      .toList

  private def downloadAndCheckTransaction[F[_]: Async: Logger](
    state:         State[F],
    transactionId: Identifier.IoTransaction32
  ): F[Identifier.IoTransaction32] =
    for {
      _                     <- Logger[F].debug(show"Fetching remote transaction id=$transactionId")
      downloadedTransaction <- downloadTransaction(state, transactionId)
      _                     <- checkTransaction(transactionId, downloadedTransaction)
      _                     <- Logger[F].debug(show"Saving transaction id=$transactionId")
      _                     <- state.transactionStore.put(transactionId, downloadedTransaction)
    } yield transactionId

  private def checkTransaction[F[_]: Async](
    transactionId:         Identifier.IoTransaction32,
    downloadedTransaction: IoTransaction
  ): F[IoTransaction] = {
    val downloadedTransactionId = downloadedTransaction.id
    if (downloadedTransactionId =!= transactionId) {
      MonadThrow[F].raiseError(TransactionHaveIncorrectId(transactionId, downloadedTransactionId))
    } else {
      downloadedTransaction.pure[F]
    }
  }

  private def downloadTransaction[F[_]: Async](
    state:         State[F],
    transactionId: Identifier.IoTransaction32
  ): F[IoTransaction] =
    OptionT(state.client.getRemoteTransaction(transactionId))
      .getOrElseF(MonadThrow[F].raiseError(TransactionNotFoundInPeer(transactionId)))

  private def startActor[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    Logger[F].info(s"Start body fetcher actor for ${state.hostId}") >>
    (state, state).pure[F]

  private def stopActor[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    Logger[F].info(s"Stop body fetcher actor for ${state.hostId}") >>
    (state, state).pure[F]
}
