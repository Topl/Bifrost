package co.topl.networking.fsnetwork

import cats.data.{NonEmptyChain, OptionT}
import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.algebras.Store
import co.topl.consensus.models.{BlockHeader, BlockId}
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.networking.fsnetwork.BlockDownloadError.{BlockBodyDownloadError, BlockHeaderDownloadError}
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.networking.fsnetwork.RequestsProxy.Message._
import co.topl.node.models.BlockBody
import org.typelevel.log4cats.Logger
import co.topl.typeclasses.implicits._
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}

object RequestsProxy {
  sealed trait Message

  object Message {
    case class SetupBlockChecker[F[_]](blockCheckerActor: BlockCheckerActor[F]) extends Message

    // blockIds shall contains chain of linked blocks, for example if we have chain A -> B -> C
    // then A is parent of B and B is parent of C
    case class DownloadHeadersRequest(hostId: HostId, blockIds: NonEmptyChain[BlockId]) extends Message

    // response shall contains chain of linked blocks, for example if we have chain A -> B -> C
    // then A is parent of B and B is parent of C
    case class DownloadHeadersResponse(
      hostId:   HostId,
      response: NonEmptyChain[(BlockId, Either[BlockHeaderDownloadError, BlockHeader])]
    ) extends Message

    // blockIds shall contains chain of linked blocks, for example if we have chain A -> B -> C
    // then A is parent of B and B is parent of C
    case class DownloadBodiesRequest(hostId: HostId, blockIds: NonEmptyChain[(BlockId, BlockHeader)]) extends Message

    // response shall contains chain of linked blocks, for example if we have chain A -> B -> C
    // then A is parent of B and B is parent of C
    case class DownloadBodiesResponse(
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
    headerRequests: Cache[BlockId, Option[BlockHeader]] =
      Caffeine.newBuilder.maximumSize(requestCacheSize).build[BlockId, Option[BlockHeader]](),
    bodyRequests: Cache[BlockId, Option[BlockBody]] =
      Caffeine.newBuilder.maximumSize(requestCacheSize).build[BlockId, Option[BlockBody]]()
  )

  type Response[F[_]] = State[F]
  type RequestsProxyActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger]: Fsm[F, State[F], Message, Response[F]] =
    Fsm {
      case (state, message: SetupBlockChecker[F] @unchecked) =>
        setupBlockChecker(state, message.blockCheckerActor)
      case (state, DownloadHeadersRequest(hostId, blockIds)) =>
        downloadHeadersRequest(state, hostId, blockIds)
      case (state, DownloadHeadersResponse(source, response)) =>
        downloadHeadersResponse(state, source, response)
      case (state, DownloadBodiesRequest(hostId, blockData)) =>
        downloadBodiesRequest(state, hostId, blockData)
      case (state, DownloadBodiesResponse(source, response)) =>
        downloadBodiesResponse(state, source, response)
    }

  def makeActor[F[_]: Async: Logger](
    reputationAggregator: ReputationAggregatorActor[F],
    peersManager:         PeersManagerActor[F],
    headerStore:          Store[F, BlockId, BlockHeader],
    bodyStore:            Store[F, BlockId, BlockBody]
  ): Resource[F, RequestsProxyActor[F]] = {
    val initialState =
      State(reputationAggregator, peersManager, blockCheckerOpt = None, headerStore, bodyStore)
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

  private def downloadHeadersRequest[F[_]: Async: Logger](
    state:    State[F],
    hostId:   HostId,
    blockIds: NonEmptyChain[BlockId]
  ): F[(State[F], Response[F])] =
    for {
      _              <- Logger[F].info(show"Get request for headers downloads $blockIds")
      idsDownloaded  <- sendAlreadyDownloadedHeadersToBlockChecker(state, hostId, blockIds)
      _              <- Logger[F].info(show"Send already downloaded header to block checker $idsDownloaded")
      idsForDownload <- sendDownloadRequestForNewHeaders(state, hostId, blockIds)
      _              <- Logger[F].info(show"Send request for additional block header download $idsForDownload")
      _ = saveDownloadRequestToCache(state.headerRequests, idsForDownload)
    } yield (state, state)

  // response with longest possible prefix for requested ids
  // where corresponding block header is already downloaded for each block id
  // also drop any data which is already in header storage
  private def sendAlreadyDownloadedHeadersToBlockChecker[F[_]: Async](
    state:    State[F],
    hostId:   HostId,
    blockIds: NonEmptyChain[BlockId]
  ): F[Seq[BlockId]] = {
    val definedPrefix: Seq[(BlockId, BlockHeader)] = getDefinedPrefixFrom(state.headerRequests, blockIds)
    sendHeaders(state, hostId, definedPrefix).getOrElse(List.empty)
  }

  // send block download request only for new headers
  private def sendDownloadRequestForNewHeaders[F[_]: Async](
    state:    State[F],
    hostId:   HostId,
    blockIds: NonEmptyChain[BlockId]
  ): F[List[BlockId]] = {
    val newBlockIds = blockIds.filterNot(state.headerRequests.contains)
    NonEmptyChain
      .fromChain(newBlockIds)
      .map { blockIds =>
        state.peersManager.sendNoWait(PeersManager.Message.BlockHeadersRequest(hostId, blockIds)) >>
        blockIds.toList.pure[F]
      }
      .getOrElse(List.empty[BlockId].pure[F])
  }

  private def downloadHeadersResponse[F[_]: Async: Logger](
    state:    State[F],
    hostId:   HostId,
    response: NonEmptyChain[(BlockId, Either[BlockHeaderDownloadError, BlockHeader])]
  ): F[(State[F], Response[F])] = {
    val successfullyDownloadedHeaders =
      response.collect { case (id, Right(header)) => (id, header) }.toList

    for {
      _       <- Logger[F].info(show"Successfully download next headers: ${successfullyDownloadedHeaders.map(_._1)}")
      _       <- processHeaderDownloadErrors(state.reputationAggregator, state.peersManager, hostId, response)
      sentIds <- sendSuccessfulHeadersPrefix(state, hostId, response)
      _       <- Logger[F].info(show"Send headers prefix to block checker $sentIds")
      _ = saveDownloadResultToCache(state.headerRequests, successfullyDownloadedHeaders)
    } yield (state, state)
  }

  private def processHeaderDownloadErrors[F[_]: Async](
    reputationAggregator: ReputationAggregatorActor[F],
    peersManager:         PeersManagerActor[F],
    source:               HostId,
    response:             NonEmptyChain[(BlockId, Either[BlockHeaderDownloadError, BlockHeader])]
  ): F[Unit] = {
    val errorsOpt =
      NonEmptyChain.fromChain(response.collect { case (id, Left(error)) => (id, error) })

    errorsOpt match {

      case Some(errors) =>
        // TODO translate error to reputation value or send error itself?
        val reputationMessage: ReputationAggregator.Message =
          ReputationAggregator.Message.UpdatePeerReputation(source, -1)

        errors.traverse { case (id, _) =>
          reputationAggregator.sendNoWait(reputationMessage) >>
          // TODO we shall not try to download header from the same host, peer manager shall decide it
          peersManager.sendNoWait(PeersManager.Message.BlockHeadersRequest(source, NonEmptyChain.one(id)))
        }.void

      case None => ().pure[F]
    }
  }

  // We send prefix to block checker if parent of first block in response is already in storage
  private def sendSuccessfulHeadersPrefix[F[_]: Async: Logger](
    state:    State[F],
    source:   HostId,
    response: NonEmptyChain[(BlockId, Either[BlockHeaderDownloadError, BlockHeader])]
  ): F[Seq[BlockId]] = {
    val firstBlock = response.head._1

    val sendMessage =
      for {
        idAndHeaders <- OptionT.fromOption[F](NonEmptyChain.fromSeq(getDefinedPrefix(response)))
        firstBlockHeader = idAndHeaders.head._2
        _       <- OptionT(state.headerStore.get(firstBlockHeader.parentHeaderId))
        _       <- OptionT.liftF(Logger[F].debug(show"Parent of header $firstBlock is already adopted"))
        sentIds <- sendHeaders(state, source, idAndHeaders.toList)
      } yield sentIds

    sendMessage.getOrElse(List.empty[BlockId])
  }

  private def downloadBodiesRequest[F[_]: Async: Logger](
    state:     State[F],
    hostId:    HostId,
    blockData: NonEmptyChain[(BlockId, BlockHeader)]
  ): F[(State[F], Response[F])] = {
    val blockIds = blockData.map(_._1)

    for {
      _              <- Logger[F].info(show"Get request for bodies downloads $blockIds")
      idsDownloaded  <- sendAlreadyDownloadedBodiesToBlockChecker(state, hostId, blockIds)
      _              <- Logger[F].info(show"Send already downloaded bodies to block checker $idsDownloaded")
      idsForDownload <- sendDownloadRequestForNewBodies(state, hostId, blockData)
      _              <- Logger[F].info(show"Send request for additional block body download $idsForDownload")
      _ = saveDownloadRequestToCache(state.bodyRequests, idsForDownload)
    } yield (state, state)
  }

  // response with longest possible prefix for requested ids
  // where corresponding block body is already downloaded for each block id
  // also drop any data which is already in body storage
  private def sendAlreadyDownloadedBodiesToBlockChecker[F[_]: Async](
    state:    State[F],
    hostId:   HostId,
    blockIds: NonEmptyChain[BlockId]
  ): F[Seq[BlockId]] = {
    val dataToSend = getDefinedPrefixFrom(state.bodyRequests, blockIds)
    sendBodies(state, hostId, dataToSend).getOrElse(List.empty)
  }

  // send block download request only for new bodies
  private def sendDownloadRequestForNewBodies[F[_]: Async](
    state:     State[F],
    hostId:    HostId,
    blockData: NonEmptyChain[(BlockId, BlockHeader)]
  ): F[List[BlockId]] = {
    val newBlockData = blockData.filterNot(d => state.bodyRequests.contains(d._1))
    NonEmptyChain
      .fromChain(newBlockData)
      .map { blockData =>
        state.peersManager.sendNoWait(PeersManager.Message.BlockBodyDownloadRequest(hostId, blockData)) >>
        blockData.map(_._1).toList.pure[F]
      }
      .getOrElse(List.empty[BlockId].pure[F])
  }

  private def downloadBodiesResponse[F[_]: Async: Logger](
    state:    State[F],
    hostId:   HostId,
    response: NonEmptyChain[(BlockId, Either[BlockBodyDownloadError, BlockBody])]
  ): F[(State[F], Response[F])] = {
    val successfullyDownloadedBodies =
      response.collect { case (id, Right(body)) => (id, body) }.toList

    for {
      _       <- Logger[F].info(show"Successfully download next bodies: ${successfullyDownloadedBodies.map(_._1)}")
      _       <- processBlockDownloadErrors(state, hostId, response)
      sentIds <- sendSuccessfulBodiesPrefix(state, hostId, response)
      _       <- Logger[F].info(show"Send bodies prefix to block checker $sentIds")
      _ = saveDownloadResultToCache(state.bodyRequests, successfullyDownloadedBodies)
    } yield (state, state)
  }

  private def processBlockDownloadErrors[F[_]: Async](
    state:    State[F],
    source:   HostId,
    response: NonEmptyChain[(BlockId, Either[BlockBodyDownloadError, BlockBody])]
  ): F[Unit] = {
    val reputationAggregator: ReputationAggregatorActor[F] = state.reputationAggregator
    val peersManager: PeersManagerActor[F] = state.peersManager

    val errorsOpt =
      NonEmptyChain.fromChain(response.collect { case (id, Left(error)) => (id, error) })

    def processError(id: BlockId): F[Unit] = {
      // TODO translate error to reputation value or send error itself?
      val reputationMessage: ReputationAggregator.Message =
        ReputationAggregator.Message.UpdatePeerReputation(source, -1)

      {
        for {
          _      <- OptionT.liftF(reputationAggregator.sendNoWait(reputationMessage))
          header <- OptionT(state.headerStore.get(id))
          message = PeersManager.Message.BlockBodyDownloadRequest(source, NonEmptyChain.one((id, header)))
          _ <- OptionT.liftF(peersManager.sendNoWait(message))
        } yield ()
      }.getOrElse(())
    }

    errorsOpt match {

      case Some(errors) => errors.traverse { case (id, _) => processError(id) }.void
      case None         => ().pure[F]
    }
  }

  // If parent of first block in response is already in storage then we send prefix to block checker
  private def sendSuccessfulBodiesPrefix[F[_]: Async: Logger](
    state:    State[F],
    source:   HostId,
    response: NonEmptyChain[(BlockId, Either[BlockBodyDownloadError, BlockBody])]
  ): F[Seq[BlockId]] = {
    val firstBlock = response.head._1

    val sendMessage =
      for {
        idAndBodies      <- OptionT.fromOption[F](NonEmptyChain.fromSeq(getDefinedPrefix(response)))
        firstBlockHeader <- OptionT(state.headerStore.get(firstBlock))
        _                <- OptionT(state.bodyStore.get(firstBlockHeader.parentHeaderId))
        _                <- OptionT.liftF(Logger[F].debug(show"Parent of block body $firstBlock is already adopted"))
        sentIds          <- sendBodies(state, source, idAndBodies.toList)
      } yield sentIds

    sendMessage.getOrElse(List.empty[BlockId])
  }

  private def sendHeaders[F[_]: Async](
    state:  State[F],
    hostId: HostId,
    toSend: Seq[(BlockId, BlockHeader)]
  ): OptionT[F, Seq[BlockId]] =
    for {
      toSend       <- OptionT(dropKnownPrefix(toSend, state.headerStore))
      blockChecker <- OptionT.fromOption[F](state.blockCheckerOpt)
      _            <- OptionT.liftF(blockChecker.sendNoWait(BlockChecker.Message.RemoteBlockHeaders(hostId, toSend)))
    } yield toSend.toList.map(_._1)

  private def sendBodies[F[_]: Async](
    state:  State[F],
    hostId: HostId,
    toSend: Seq[(BlockId, BlockBody)]
  ): OptionT[F, Seq[BlockId]] =
    for {
      toSend       <- OptionT(dropKnownPrefix(toSend, state.bodyStore))
      blockChecker <- OptionT.fromOption[F](state.blockCheckerOpt)
      _            <- OptionT.liftF(blockChecker.sendNoWait(BlockChecker.Message.RemoteBlockBodies(hostId, toSend)))
    } yield toSend.toList.map(_._1)

  private def getDefinedPrefix[T, E](blocks: NonEmptyChain[(BlockId, Either[E, T])]): Seq[(BlockId, T)] =
    blocks.takeWhile_ { case (_, res) => res.isRight }.collect { case (id, Right(body)) => (id, body) }

  private def getDefinedPrefixFrom[T](
    requestsMap: Cache[BlockId, Option[T]],
    request:     NonEmptyChain[BlockId]
  ): Seq[(BlockId, T)] =
    request
      .map(id => (id, requestsMap.get(id).flatten))
      .takeWhile_ { case (_, bodyOpt) => bodyOpt.isDefined }
      .map { case (id, bodyOpt) => (id, bodyOpt.get) }

  private def saveDownloadRequestToCache[T](cache: Cache[BlockId, Option[T]], ids: Seq[BlockId]) =
    ids.map(id => cache.put(id, None))

  private def saveDownloadResultToCache[T](cache: Cache[BlockId, Option[T]], ids: Seq[(BlockId, T)]) =
    ids.map { case (id, data) => cache.put(id, Option(data)) }
}
