package co.topl.networking.fsnetwork

import cats.data.{NonEmptyChain, OptionT}
import cats.effect.{Async, Resource}
import cats.implicits._
import cats.{Applicative, MonadThrow}
import co.topl.actor.{Actor, Fsm}
import co.topl.algebras.Store
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.models.{BlockBody, Transaction, TypedIdentifier}
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.typeclasses.implicits._
import fs2.Stream
import org.typelevel.log4cats.Logger

object PeerBlockBodyFetcher {
  sealed trait Message

  object Message {
    case class DownloadBlocks(blocks: NonEmptyChain[TypedIdentifier]) extends Message
    case object StopActor extends Message
    case object StartActor extends Message
  }

  case class State[F[_]](
    hostId:           HostId,
    client:           BlockchainPeerClient[F],
    blockChecker:     BlockCheckerActor[F],
    transactionStore: Store[F, TypedIdentifier, Transaction]
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
    blockChecker:     BlockCheckerActor[F],
    transactionStore: Store[F, TypedIdentifier, Transaction]
  ): Resource[F, PeerBlockBodyFetcherActor[F]] = {
    val initialState = State(hostId, client, blockChecker, transactionStore)
    Actor.make(initialState, getFsm[F])
  }

  private def downloadBodies[F[_]: Async: Logger](
    state:            State[F],
    blocksToDownload: NonEmptyChain[TypedIdentifier]
  ): F[(State[F], Response[F])] =
    for {
      idToBody <- Stream.foldable(blocksToDownload).evalMap(downloadBlockBody(state)).compile.toList
      messageToSend = BlockChecker.Message.RemoteBlockBodies(state.hostId, NonEmptyChain.fromSeq(idToBody).get)
      _ <- state.blockChecker.sendNoWait(messageToSend)
    } yield (state, state)

  private def downloadBlockBody[F[_]: Async: Logger](state: State[F])(blockId: TypedIdentifier) =
    for {
      _               <- Logger[F].info(show"Fetching remote body id=$blockId")
      body: BlockBody <- OptionT(state.client.getRemoteBody(blockId)).getOrNoSuchElement(blockId.show)
      _               <- Logger[F].debug(show"Fetched remote body id=$blockId")
      _               <- downloadMissingTransactions(state, body).compile.drain
    } yield (blockId, body)

  private def downloadMissingTransactions[F[_]: Async: Logger](state: State[F], blockBody: BlockBody): Stream[F, Unit] =
    Stream
      .iterable(blockBody)
      .evalMap(transactionId =>
        state.transactionStore
          .contains(transactionId)
          .ifM(ifTrue = Applicative[F].unit, ifFalse = downloadTransaction(state, transactionId))
      )

  private def downloadTransaction[F[_]: Async: Logger](state: State[F], transactionId: TypedIdentifier) = {
    for {
      _ <- Logger[F].debug(show"Fetching remote transaction id=$transactionId")
      transaction <- OptionT(state.client.getRemoteTransaction(transactionId)).getOrNoSuchElement(transactionId.show)
      _ <- MonadThrow[F].raiseWhen(transaction.id.asTypedBytes =!= transactionId)(InconsistentTransactionId)
      _ <- Logger[F].debug(show"Saving transaction id=$transactionId")
      _ <- state.transactionStore.put(transactionId, transaction)
    } yield ()
  }

  private def startActor[F[_]: Applicative: Logger](state: State[F]): F[(State[F], Response[F])] =
    Logger[F].info(s"Start body fetcher actor for ${state.hostId}") *>
    (state, state).pure[F]

  private def stopActor[F[_]: Applicative: Logger](state: State[F]): F[(State[F], Response[F])] =
    Logger[F].info("Stop body fetcher actor for ${state.hostId}") *>
    (state, state).pure[F]
}
