package co.topl.networking.fsnetwork

import cats.data.{NonEmptyChain, OptionT}
import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.algebras.Store
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.models.{BlockHeader, BlockId}
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.networking.fsnetwork.RequestsProxy.Message._
import co.topl.node.models.BlockBody
import org.typelevel.log4cats.Logger
import co.topl.typeclasses.implicits._

object RequestsProxy {
  sealed trait Message

  object Message {
    case class SetupBlockChecker[F[_]](blockCheckerActor: BlockCheckerActor[F]) extends Message

    case class DownloadBlocksRequest(hostId: HostId, blockIds: NonEmptyChain[BlockId]) extends Message

    case class DownloadBlockResponse(
      source:   HostId,
      response: NonEmptyChain[(BlockId, Either[BlockBodyDownloadError, BlockBody])]
    ) extends Message
  }

  case class State[F[_]](
    reputationAggregator: ReputationAggregatorActor[F],
    peersManager:         PeersManagerActor[F],
    blockCheckerOpt:      Option[BlockCheckerActor[F]],
    headerStore:          Store[F, BlockId, BlockHeader],
    bodyStore:            Store[F, BlockId, BlockBody],
    bodyRequests:         Map[BlockId, Option[BlockBody]] = Map.empty
  )

  type Response[F[_]] = State[F]
  type RequestsProxyActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger]: Fsm[F, State[F], Message, Response[F]] =
    Fsm {
      case (state, message: SetupBlockChecker[F] @unchecked) =>
        setupBlockChecker(state, message.blockCheckerActor)
      case (state, DownloadBlocksRequest(hostId, blockIds)) =>
        downloadBlockRequest(state, hostId, blockIds)
      case (state, DownloadBlockResponse(source, response)) =>
        downloadBlockResponse(state, source, response)
    }

  def makeActor[F[_]: Async: Logger](
    reputationAggregator: ReputationAggregatorActor[F],
    peersManager:         PeersManagerActor[F],
    headerStore:          Store[F, BlockId, BlockHeader],
    bodyStore:            Store[F, BlockId, BlockBody]
  ): Resource[F, RequestsProxyActor[F]] = {
    val initialState =
      State(
        reputationAggregator,
        peersManager,
        blockCheckerOpt = None,
        headerStore,
        bodyStore
      )
    Actor.make(initialState, getFsm[F])
  }

  private def setupBlockChecker[F[_]: Async: Logger](
    state:        State[F],
    blockChecker: BlockCheckerActor[F]
  ): F[(State[F], Response[F])] = {
    val newState = state.copy(blockCheckerOpt = Option(blockChecker))
    Logger[F].info("Setup block checker for RequestsProxy") >>
    (newState, newState).pure[F]
  }

  private def downloadBlockRequest[F[_]: Async: Logger](
    state:    State[F],
    hostId:   HostId,
    blockIds: NonEmptyChain[BlockId]
  ): F[(State[F], Response[F])] =
    for {
      _              <- Logger[F].info(show"Get request for blocks downloads $blockIds")
      idsDownloaded  <- sendAlreadyDownloadedBlocksToBlockChecker(state, hostId, blockIds)
      _              <- Logger[F].info(show"Send already downloaded block to block checker $idsDownloaded")
      idsForDownload <- sendDownloadRequestForNewBlocks(state, hostId, blockIds)
      _              <- Logger[F].info(show"Send request for additional block body download $idsForDownload")
      bodyReq: Map[BlockId, Option[BlockBody]] = state.bodyRequests -- idsDownloaded ++ idsForDownload.map((_, None))
      newState = state.copy(bodyRequests = bodyReq)
    } yield (newState, newState)

  // response with longest possible prefix for request ids
  // where for each block id corresponding block body is already downloaded
  private def sendAlreadyDownloadedBlocksToBlockChecker[F[_]: Async](
    state:    State[F],
    hostId:   HostId,
    blockIds: NonEmptyChain[BlockId]
  ): F[List[BlockId]] = {
    val alreadyDownloadedBodies =
      blockIds
        .map(id => (id, state.bodyRequests.get(id).flatten))
        .takeWhile_ { case (_, bodyOpt) => bodyOpt.isDefined }
        .map { case (id, bodyOpt) => (id, bodyOpt.get) }

    // TODO add meta information about downloaded blocks so we could get actual source of block
    val sendMessage: Option[F[Unit]] =
      for {
        downloadedPrefix <- NonEmptyChain.fromSeq(alreadyDownloadedBodies)
        blockChecker     <- state.blockCheckerOpt
      } yield blockChecker.sendNoWait(BlockChecker.Message.RemoteBlockBodies(hostId, downloadedPrefix))

    sendMessage.getOrElse(().pure[F]) >> alreadyDownloadedBodies.map(_._1).pure[F]
  }

  // send block download request only for new blocks
  private def sendDownloadRequestForNewBlocks[F[_]: Async](
    state:    State[F],
    hostId:   HostId,
    blockIds: NonEmptyChain[BlockId]
  ): F[List[BlockId]] = {
    val newBlockIds = blockIds.filterNot(state.bodyRequests.contains)
    NonEmptyChain
      .fromChain(newBlockIds)
      .map { blockIds =>
        state.peersManager.sendNoWait(PeersManager.Message.BlockDownloadRequest(hostId, blockIds)) >>
        blockIds.toList.pure[F]
      }
      .getOrElse(List.empty[BlockId].pure[F])
  }

  private def downloadBlockResponse[F[_]: Async: Logger](
    state:    State[F],
    hostId:   HostId,
    response: NonEmptyChain[(BlockId, Either[BlockBodyDownloadError, BlockBody])]
  ): F[(State[F], Response[F])] = {
    val successfullyDownloadedBlocks =
      response.collect { case (id, Right(body)) => (id, Option(body)) }

    for {
      _       <- Logger[F].info(show"Successfully download next blocks: ${successfullyDownloadedBlocks.map(_._1)}")
      _       <- processDownloadErrors(state.reputationAggregator, state.peersManager, hostId, response)
      sentIds <- sendSuccessfulBodiesPrefix(state.headerStore, state.bodyStore, state.blockCheckerOpt, hostId, response)
      _       <- Logger[F].info(show"Send bodies prefix to block checker $sentIds")
      bodyReq: Map[BlockId, Option[BlockBody]] = state.bodyRequests -- sentIds ++ successfullyDownloadedBlocks.toList
      newState = state.copy(bodyRequests = bodyReq)
    } yield (newState, newState)
  }

  private def processDownloadErrors[F[_]: Async](
    reputationAggregator: ReputationAggregatorActor[F],
    peersManager:         PeersManagerActor[F],
    source:               HostId,
    response:             NonEmptyChain[(BlockId, Either[BlockBodyDownloadError, BlockBody])]
  ): F[Unit] = {
    val errorsOpt =
      NonEmptyChain.fromChain(response.collect { case (id, Left(error)) => (id, error) })

    errorsOpt match {

      case Some(errors) =>
        // TODO translate error to reputation value or send error itself?
        val reputationMessage: ReputationAggregator.Message =
          ReputationAggregator.Message.UpdatePeerReputation(source, -1)
        // TODO we shall not try to download body from the same host, peer manager shall decide it
        val repeatDownloadMessage: PeersManager.Message =
          PeersManager.Message.BlockDownloadRequest(source, errors.map(_._1))

        errors.traverse(_ => reputationAggregator.sendNoWait(reputationMessage)) >>
        peersManager.sendNoWait(repeatDownloadMessage)

      case None => ().pure[F]
    }
  }

  // If parent of first block in response is already in storage then we send prefix to block checker
  private def sendSuccessfulBodiesPrefix[F[_]: Async: Logger](
    headerStore:     Store[F, BlockId, BlockHeader],
    bodyStore:       Store[F, BlockId, BlockBody],
    blockCheckerOpt: Option[BlockCheckerActor[F]],
    source:          HostId,
    response:        NonEmptyChain[(BlockId, Either[BlockBodyDownloadError, BlockBody])]
  ): F[List[BlockId]] = {
    val firstBlock = response.head._1
    def prefix =
      response.takeWhile_ { case (_, res) => res.isRight }.collect { case (id, Right(body)) => (id, body) }

    val sendMessage =
      for {
        firstBlockHeader <- OptionT(headerStore.get(firstBlock))
        _                <- OptionT(bodyStore.get(firstBlockHeader.parentHeaderId))
        _                <- OptionT.liftF(Logger[F].debug(show"Parent of block body $firstBlock is already adopted"))
        blockChecker     <- OptionT.fromOption[F](blockCheckerOpt)
        idAndBodies      <- OptionT.fromOption[F](NonEmptyChain.fromSeq(prefix))
      } yield blockChecker.sendNoWait(BlockChecker.Message.RemoteBlockBodies(source, idAndBodies)) >>
      idAndBodies.map(_._1).toList.pure[F]

    sendMessage.getOrElse(List.empty[BlockId].pure[F]).flatten
  }
}
