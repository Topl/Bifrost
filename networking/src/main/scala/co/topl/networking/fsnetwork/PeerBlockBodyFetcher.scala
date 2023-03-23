package co.topl.networking.fsnetwork

import cats.data.{NonEmptyChain, OptionT}
import cats.effect.{Async, Resource}
import cats.implicits._
import cats.{Applicative, MonadThrow}
import co.topl.actor.{Actor, Fsm}
import co.topl.algebras.Store
import co.topl.brambl.models.Identifier
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.models.BlockId
import co.topl.networking.blockchain.BlockchainPeerClient
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

  private def downloadBlockBody[F[_]: Async: Logger](state: State[F])(blockId: BlockId) =
    for {
      _               <- Logger[F].info(show"Fetching remote body id=$blockId")
      body: BlockBody <- OptionT(state.client.getRemoteBody(blockId)).getOrNoSuchElement(blockId.show)
      _               <- Logger[F].debug(show"Fetched remote body id=$blockId")
      _               <- downloadMissingTransactions(state, body).compile.drain
    } yield (blockId, Either.right[BlockBodyDownloadError, BlockBody](body))

  private def downloadMissingTransactions[F[_]: Async: Logger](state: State[F], blockBody: BlockBody): Stream[F, Unit] =
    Stream
      .iterable[F, Identifier.IoTransaction32](blockBody.transactionIds)
      .evalMap(transactionId =>
        state.transactionStore
          .contains(transactionId)
          .ifM(ifTrue = Applicative[F].unit, ifFalse = downloadTransaction(state, transactionId))
      )

  private def downloadTransaction[F[_]: Async: Logger](state: State[F], transactionId: Identifier.IoTransaction32) = {
    val InconsistentTransactionId =
      new IllegalArgumentException("Claimed transaction ID did not match provided header")

    for {
      _           <- Logger[F].debug(show"Fetching remote transaction id=$transactionId")
      transaction <- OptionT(state.client.getRemoteTransaction(transactionId)).getOrNoSuchElement(transactionId.show)
      _           <- MonadThrow[F].raiseWhen(transaction.id =!= transactionId)(InconsistentTransactionId)
      _           <- Logger[F].debug(show"Saving transaction id=$transactionId")
      _           <- state.transactionStore.put(transactionId, transaction)
    } yield ()
  }

  private def startActor[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    Logger[F].info(s"Start body fetcher actor for ${state.hostId}") >>
    (state, state).pure[F]

  private def stopActor[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    Logger[F].info(s"Stop body fetcher actor for ${state.hostId}") >>
    (state, state).pure[F]
}
