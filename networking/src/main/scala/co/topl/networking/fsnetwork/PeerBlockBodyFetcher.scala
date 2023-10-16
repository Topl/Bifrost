package co.topl.networking.fsnetwork

import cats.MonadThrow
import cats.data.{EitherT, NonEmptyChain, OptionT}
import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.algebras.Store
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.algebras.BlockHeaderToBodyValidationAlgebra
import co.topl.consensus.models.BlockHeaderToBodyValidationFailure.IncorrectTxRoot
import co.topl.consensus.models.{BlockHeader, BlockId}
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockDownloadError.BlockBodyOrTransactionError
import co.topl.networking.fsnetwork.BlockDownloadError.BlockBodyOrTransactionError._
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.node.models.{Block, BlockBody}
import co.topl.models.utility._
import co.topl.typeclasses.implicits._
import fs2.Stream
import org.typelevel.log4cats.Logger

object PeerBlockBodyFetcher {
  sealed trait Message

  object Message {
    case object StartActor extends Message
    case object StopActor extends Message

    /**
     * Request to download block bodies from peer, downloaded bodies will be sent to block checker directly
     *
     * @param blockHeaders bodies block header to download
     */
    case class DownloadBlocks(blockHeaders: NonEmptyChain[BlockHeader]) extends Message
  }

  case class State[F[_]](
    hostId:                 HostId,
    client:                 BlockchainPeerClient[F],
    requestsProxy:          RequestsProxyActor[F],
    transactionStore:       Store[F, TransactionId, IoTransaction],
    headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F],
    transactionFetcher:     TransactionFetcher[F]
  )

  type Response[F[_]] = State[F]
  type PeerBlockBodyFetcherActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger]: Fsm[F, State[F], Message, Response[F]] = Fsm {
    case (state, Message.DownloadBlocks(blockHeadersToDownload)) => downloadBodies(state, blockHeadersToDownload)
    case (state, Message.StartActor)                             => startActor(state)
    case (state, Message.StopActor)                              => stopActor(state)
  }

  def makeActor[F[_]: Async: Logger](
    hostId:                      HostId,
    client:                      BlockchainPeerClient[F],
    requestsProxy:               RequestsProxyActor[F],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F]
  ): Resource[F, PeerBlockBodyFetcherActor[F]] = {
    val transactionFetcher =
      new TransactionFetcher[F](hostId, transactionSyntaxValidation, transactionStore, client, false)
    val initialState =
      State(hostId, client, requestsProxy, transactionStore, headerToBodyValidation, transactionFetcher)

    val actorName = s"Body fetcher actor for peer $hostId"
    Actor.makeWithFinalize(actorName, initialState, getFsm[F], finalizer[F])
  }

  private def downloadBodies[F[_]: Async: Logger](
    state:                  State[F],
    blockHeadersToDownload: NonEmptyChain[BlockHeader]
  ): F[(State[F], Response[F])] =
    for {
      headerToBody <- Stream.foldable(blockHeadersToDownload).evalMap(downloadBlockBody(state)).compile.toList
      message = RequestsProxy.Message.DownloadBodiesResponse(state.hostId, NonEmptyChain.fromSeq(headerToBody).get)
      _ <- state.requestsProxy.sendNoWait(message)
    } yield (state, state)

  private def downloadBlockBody[F[_]: Async: Logger](
    state: State[F]
  )(blockHeader: BlockHeader): F[(BlockHeader, Either[BlockBodyOrTransactionError, UnverifiedBlockBody])] = {
    val blockId = blockHeader.id

    val body: F[UnverifiedBlockBody] =
      for {
        _                 <- Logger[F].info(show"Fetching remote body id=$blockId from peer ${state.hostId}")
        bodyStart         <- System.currentTimeMillis().pure[F]
        body              <- downloadBlockBody(state, blockId)
        bodyEnd           <- System.currentTimeMillis().pure[F]
        _                 <- Logger[F].info(show"Fetched remote body id=$blockId from peer ${state.hostId}")
        _                 <- checkBody(state, Block(blockHeader, body))
        txAndDownloadTime <- downloadingMissingTransactions(state, body)
        allTxDownloadTime = txAndDownloadTime.collect { case (_, Some(time)) => time }
      } yield UnverifiedBlockBody(state.hostId, body, bodyEnd - bodyStart, allTxDownloadTime)

    body
      .map(blockBody => Either.right[BlockBodyOrTransactionError, UnverifiedBlockBody](blockBody))
      .handleError {
        case e: BlockBodyOrTransactionError => Either.left[BlockBodyOrTransactionError, UnverifiedBlockBody](e)
        case unknownError                   => Either.left(UnknownError(unknownError))
      }
      .flatTap {
        case Right(_) =>
          Logger[F].debug(show"Successfully download block $blockId from peer ${state.hostId}")
        case Left(error) =>
          Logger[F].error(show"Failed download block $blockId from peer ${state.hostId} because of: ${error.toString}")
      }
      .map((blockHeader, _))
  }

  private def downloadBlockBody[F[_]: Async](state: State[F], blockId: BlockId): F[BlockBody] =
    OptionT(state.client.getRemoteBody(blockId)).getOrElseF(MonadThrow[F].raiseError(BodyNotFoundInPeer))

  private def checkBody[F[_]: Async](state: State[F], block: Block): F[BlockBody] =
    EitherT(state.headerToBodyValidation.validate(block))
      .map(_.body)
      .leftMap { case e: IncorrectTxRoot =>
        BodyHaveIncorrectTxRoot(e.headerTxRoot, e.bodyTxRoot)
      }
      .rethrowT

  private def downloadingMissingTransactions[F[_]: Async](
    state:     State[F],
    blockBody: BlockBody
  ): F[List[(TransactionId, Option[Long])]] =
    Stream
      .iterable[F, TransactionId](blockBody.allTransactionIds)
      .evalMap(transactionId =>
        state.transactionStore
          .contains(transactionId)
          .flatMap {
            case true  => (transactionId, Option.empty[Long]).pure[F]
            case false => state.transactionFetcher.downloadCheckSaveTransaction(transactionId)
          }
      )
      .compile
      .toList

  private def startActor[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    Logger[F].info(show"Start body fetcher actor for peer ${state.hostId}") >>
    (state, state).pure[F]

  private def stopActor[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    Logger[F].info(show"Stop body fetcher actor for peer ${state.hostId}") >>
    (state, state).pure[F]

  private def finalizer[F[_]: Async: Logger](state: State[F]): F[Unit] = stopActor(state).void
}
