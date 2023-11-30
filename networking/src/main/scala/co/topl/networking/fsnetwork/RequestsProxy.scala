package co.topl.networking.fsnetwork

import cats.data.{NonEmptyChain, OptionT}
import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.algebras.Store
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData, SlotId}
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.networking.fsnetwork.BlockDownloadError.{BlockBodyOrTransactionError, BlockHeaderDownloadError}
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.RequestsProxy.Message._
import co.topl.typeclasses.implicits._
import co.topl.node.models.BlockBody
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import org.typelevel.log4cats.Logger
import scalacache.Entry
import scalacache.caffeine.CaffeineCache

import scala.annotation.nowarn
import scala.concurrent.duration.DurationInt

object RequestsProxy {
  sealed trait Message

  object Message {
    case class SetupBlockChecker[F[_]](blockCheckerActor: BlockCheckerActor[F]) extends Message

    case object ResetRequestsProxy extends Message

    case class RemoteSlotData(hostId: HostId, slotData: NonEmptyChain[SlotData]) extends Message

    case class BadKLookbackSlotData(hostId: HostId) extends Message

    // blockIds shall contains chain of linked blocks, for example if we have chain A -> B -> C
    // then A is parent of B and B is parent of C
    case class DownloadHeadersRequest(hostIdTip: HostId, blockIds: NonEmptyChain[BlockId]) extends Message

    // response shall contains chain of linked blocks, for example if we have chain A -> B -> C
    // then A is parent of B and B is parent of C
    case class DownloadHeadersResponse(
      hostId:   HostId,
      response: NonEmptyChain[(BlockId, Either[BlockHeaderDownloadError, UnverifiedBlockHeader])]
    ) extends Message

    // blockIds shall contains chain of linked blocks, for example if we have chain A -> B -> C
    // then A is parent of B and B is parent of C
    case class DownloadBodiesRequest(hostIdTip: HostId, blockHeaders: NonEmptyChain[BlockHeader]) extends Message

    // response shall contains chain of linked blocks, for example if we have chain A -> B -> C
    // then A is parent of B and B is parent of C
    case class DownloadBodiesResponse(
      source:   HostId,
      response: NonEmptyChain[(BlockHeader, Either[BlockBodyOrTransactionError, UnverifiedBlockBody])]
    ) extends Message

    // If verification of block is failed then we shall no longer process that particular blockId
    case class InvalidateBlockId(source: HostId, blockId: BlockId) extends Message
  }

  case class State[F[_]](
    peersManager:     PeersManagerActor[F],
    blockCheckerOpt:  Option[BlockCheckerActor[F]],
    headerStore:      Store[F, BlockId, BlockHeader],
    bodyStore:        Store[F, BlockId, BlockBody],
    headerRequests:   CaffeineCache[F, BlockId, Option[UnverifiedBlockHeader]],
    bodyRequests:     CaffeineCache[F, BlockId, Option[UnverifiedBlockBody]],
    slotDataResponse: CaffeineCache[F, SlotId, Unit]
  )

  type Response[F[_]] = State[F]
  type RequestsProxyActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger]: Fsm[F, State[F], Message, Response[F]] =
    Fsm {
      case (state, message: SetupBlockChecker[F] @unchecked)    => setupBlockChecker(state, message.blockCheckerActor)
      case (state, ResetRequestsProxy)                          => resetProxy(state)
      case (state, RemoteSlotData(hostId, slotData))            => processRemoteSlotData(state, hostId, slotData)
      case (state, BadKLookbackSlotData(hostId))                => resendBadKLookbackSlotData(state, hostId)
      case (state, DownloadHeadersRequest(hostId, blockIds))    => downloadHeadersRequest(state, hostId, blockIds)
      case (state, DownloadHeadersResponse(source, response))   => downloadHeadersResponse(state, source, response)
      case (state, DownloadBodiesRequest(hostId, blockHeaders)) => downloadBodiesRequest(state, hostId, blockHeaders)
      case (state, DownloadBodiesResponse(source, response))    => downloadBodiesResponse(state, source, response)
      case (state, InvalidateBlockId(source, blockId))          => invalidateBlockId(state, source, blockId)
    }

  def makeActor[F[_]: Async: Logger](
    peersManager: PeersManagerActor[F],
    headerStore:  Store[F, BlockId, BlockHeader],
    bodyStore:    Store[F, BlockId, BlockBody],
    headerRequests: Cache[BlockId, Entry[Option[UnverifiedBlockHeader]]] =
      Caffeine.newBuilder.maximumSize(requestCacheSize).build[BlockId, Entry[Option[UnverifiedBlockHeader]]](),
    bodyRequests: Cache[BlockId, Entry[Option[UnverifiedBlockBody]]] =
      Caffeine.newBuilder.maximumSize(requestCacheSize).build[BlockId, Entry[Option[UnverifiedBlockBody]]](),
    slotDataResponse: Cache[SlotId, Entry[Unit]] =
      Caffeine.newBuilder.maximumSize(slotIdResponseCacheSize).build[SlotId, Entry[Unit]]()
  ): Resource[F, RequestsProxyActor[F]] = {
    val initialState =
      State(
        peersManager,
        blockCheckerOpt = None,
        headerStore,
        bodyStore,
        CaffeineCache(headerRequests),
        CaffeineCache(bodyRequests),
        CaffeineCache(slotDataResponse)
      )
    val actorName = "Requests proxy actor"
    Actor.make(actorName, initialState, getFsm[F])
  }

  private def setupBlockChecker[F[_]: Async: Logger](
    state:        State[F],
    blockChecker: BlockCheckerActor[F]
  ): F[(State[F], Response[F])] = {
    val newState = state.copy(blockCheckerOpt = Option(blockChecker))
    Logger[F].info("Setup block checker for RequestsProxy") >>
    (newState, newState).pure[F]
  }

  private def resetProxy[F[_]: Async](state: State[F]): F[(State[F], Response[F])] =
    for {
      _ <- state.peersManager.sendNoWait(PeersManager.Message.GetCurrentTips)
      _ <- state.headerRequests.doRemoveAll
      _ <- state.bodyRequests.doRemoveAll
    } yield (state, state)

  private def processRemoteSlotData[F[_]: Async: Logger](
    state:    State[F],
    source:   HostId,
    slotData: NonEmptyChain[SlotData]
  ): F[(State[F], Response[F])] = {
    val processedSlotId = slotData.last.slotId
    if (state.slotDataResponse.underlying.contains(processedSlotId)) {
      Logger[F].info(show"Ignore already send slot data with last id $processedSlotId") >>
      (state, state).pure[F]
    } else {
      val message = BlockChecker.Message.RemoteSlotData(source, slotData)
      state.slotDataResponse.put(processedSlotId)(()) >>
      state.blockCheckerOpt.traverse_(blockChecker => blockChecker.sendNoWait(message)) >>
      (state, state).pure[F]
    }
  }

  private def resendBadKLookbackSlotData[F[_]: Async: Logger](
    state:  State[F],
    source: HostId
  ): F[(State[F], Response[F])] =
    Logger[F].warn(show"Received declined remote slot data chain because of low density") >>
    state.peersManager.sendNoWait(PeersManager.Message.BadKLookbackSlotData(source)) >>
    (state, state).pure[F]

  private def downloadHeadersRequest[F[_]: Async: Logger](
    state:    State[F],
    source:   HostId,
    blockIds: NonEmptyChain[BlockId]
  ): F[(State[F], Response[F])] =
    for {
      _              <- Logger[F].info(show"Get request for headers downloads $blockIds")
      idsDownloaded  <- sendAlreadyDownloadedHeadersToBlockChecker(state, blockIds)
      _              <- Logger[F].info(show"Send already downloaded header to block checker $idsDownloaded")
      idsForDownload <- sendDownloadRequestForNewHeaders(state, source, blockIds)
      _              <- Logger[F].info(show"Send request for additional block header download $idsForDownload")
      _              <- saveDownloadRequestToCache(state.headerRequests, idsForDownload)
    } yield (state, state)

  // response with longest possible prefix for requested ids
  // where corresponding block header is already downloaded for each block id
  // also drop any data which is already in header storage
  private def sendAlreadyDownloadedHeadersToBlockChecker[F[_]: Async: Logger](
    state:    State[F],
    blockIds: NonEmptyChain[BlockId]
  ): F[Seq[BlockId]] = {
    val sentIdsOpt =
      for {
        availablePrefix <- OptionT.liftF(getAvailablePrefix(state.headerRequests, blockIds)(identity))
        newIdAndHeader  <- OptionT(dropKnownPrefix(availablePrefix, state.headerStore)(identity))
        sentIds         <- sendHeaders(state, newIdAndHeader.map(_._2))
      } yield sentIds.toList.map(_.id)

    sentIdsOpt.getOrElse(Seq.empty)
  }

  // send block download request only for new headers
  private def sendDownloadRequestForNewHeaders[F[_]: Async](
    state:    State[F],
    source:   HostId,
    blockIds: NonEmptyChain[BlockId]
  ): F[List[BlockId]] = {
    val newBlockIds = blockIds.filterNot(state.headerRequests.underlying.contains)
    NonEmptyChain
      .fromChain(newBlockIds)
      .map { blockIds =>
        state.peersManager.sendNoWait(PeersManager.Message.BlockHeadersRequest(source.some, blockIds)) >>
        blockIds.toList.pure[F]
      }
      .getOrElse(List.empty[BlockId].pure[F])
  }

  private def downloadHeadersResponse[F[_]: Async: Logger](
    state:    State[F],
    source:   HostId,
    response: NonEmptyChain[(BlockId, Either[BlockHeaderDownloadError, UnverifiedBlockHeader])]
  ): F[(State[F], Response[F])] = {
    val responseLog =
      response.map { case (id, res) => show"$id with res ${res.isRight}" }.mkString_(", ")

    val successfullyDownloadedHeaders =
      response.collect { case (id, Right(header)) => (id, header) }.toList

    for {
      _       <- Logger[F].info(show"Download next headers: $responseLog")
      _       <- processHeaderDownloadPerformance(state.peersManager, response)
      _       <- processHeaderDownloadErrors(state.peersManager, source, response)
      sentIds <- sendSuccessfulHeadersPrefix(state, successfullyDownloadedHeaders)
      _       <- Logger[F].info(show"Send headers prefix to block checker $sentIds")
      _       <- saveDownloadResultToCache(state.headerRequests, successfullyDownloadedHeaders)
    } yield (state, state)
  }

  private def processHeaderDownloadPerformance[F[_]: Async](
    peersManager: PeersManagerActor[F],
    response:     NonEmptyChain[(BlockId, Either[BlockHeaderDownloadError, UnverifiedBlockHeader])]
  ): F[Unit] =
    response
      .collect { case (_, Right(header)) => header }
      .traverse { h =>
        peersManager.sendNoWait(PeersManager.Message.DownloadTimeHeader(h.source, h.downloadTimeMs))
      }
      .void

  private def processHeaderDownloadErrors[F[_]: Async](
    peersManager: PeersManagerActor[F],
    source:       HostId,
    response:     NonEmptyChain[(BlockId, Either[BlockHeaderDownloadError, UnverifiedBlockHeader])]
  ): F[Unit] = {
    val errorsOpt =
      NonEmptyChain.fromChain(response.collect { case (id, Left(error)) => (id, error) })

    errorsOpt match {
      case Some(errors) =>
        errors
          .forall(_._2.notCritical)
          .pure[F]
          .ifM(
            ifTrue = peersManager.sendNoWait(PeersManager.Message.NonCriticalErrorForHost(source)),
            ifFalse = peersManager.sendNoWait(PeersManager.Message.CriticalErrorForHost(source))
          ) >>
        peersManager.sendNoWait(PeersManager.Message.BlockHeadersRequest(None, errors.map(_._1)))

      case None => ().pure[F]
    }
  }

  // Send to block checker first available header for checking,
  // i.e. successfully downloaded header with already stored parent.
  // We can't send whole response at once because some parents could be missing
  private def sendSuccessfulHeadersPrefix[F[_]: Async: Logger](
    state:                         State[F],
    successfullyDownloadedHeaders: List[(BlockId, UnverifiedBlockHeader)]
  ): F[Seq[BlockId]] = {
    val sendMessage =
      for {
        idAndHeaders    <- OptionT.fromOption[F](NonEmptyChain.fromSeq(successfullyDownloadedHeaders))
        newIdAndHeaders <- OptionT(dropKnownPrefix(idAndHeaders.toList, state.headerStore)(identity))
        (firstNewId, unverifiedHeader) = newIdAndHeaders.head
        _           <- OptionT(state.headerStore.get(unverifiedHeader.blockHeader.parentHeaderId))
        _           <- OptionT.liftF(Logger[F].debug(show"Parent of header $firstNewId is already adopted"))
        sentHeaders <- sendHeaders(state, NonEmptyChain.one(unverifiedHeader))
      } yield sentHeaders.toList.map(_.id)

    sendMessage.getOrElse(Seq.empty[BlockId])
  }

  private def downloadBodiesRequest[F[_]: Async: Logger](
    state:   State[F],
    hostId:  HostId,
    headers: NonEmptyChain[BlockHeader]
  ): F[(State[F], Response[F])] = {
    val blockIds = headers.map(_.id)

    for {
      _           <- Logger[F].info(show"Get request for bodies downloads $blockIds")
      downloaded  <- sendAlreadyDownloadedBodiesToBlockChecker(state, headers)
      _           <- Logger[F].info(show"Send already downloaded bodies to block checker $downloaded")
      forDownload <- sendDownloadRequestForNewBodies(state, hostId, headers)
      _           <- Logger[F].info(show"Send request for additional block body download $forDownload")
      _           <- saveDownloadRequestToCache(state.bodyRequests, forDownload)
    } yield (state, state)
  }

  // response with longest possible prefix for requested ids
  // where corresponding block body is already downloaded for each block id
  // also drop any data which is already in body storage
  private def sendAlreadyDownloadedBodiesToBlockChecker[F[_]: Async](
    state:    State[F],
    blockIds: NonEmptyChain[BlockHeader]
  ): F[Seq[BlockId]] = {
    val sentIdsOpt =
      for {
        dataToSend        <- OptionT.liftF(getAvailablePrefix(state.bodyRequests, blockIds)(_.id))
        newBlockIdAndData <- OptionT(dropKnownPrefix(dataToSend, state.bodyStore)(_.id))
        sentIds           <- sendBodies(state, newBlockIdAndData)
      } yield sentIds.map(_.id)

    sentIdsOpt.getOrElse(List.empty)
  }

  // send block download request only for new bodies
  private def sendDownloadRequestForNewBodies[F[_]: Async](
    state:     State[F],
    hostId:    HostId,
    blockData: NonEmptyChain[BlockHeader]
  ): F[List[BlockId]] = {
    val newBlockData = blockData.filterNot(d => state.bodyRequests.underlying.contains(d.id))
    NonEmptyChain
      .fromChain(newBlockData)
      .map { blockData =>
        state.peersManager.sendNoWait(PeersManager.Message.BlockBodyRequest(hostId.some, blockData)) >>
        blockData.map(_.id).toList.pure[F]
      }
      .getOrElse(List.empty[BlockId].pure[F])
  }

  private def downloadBodiesResponse[F[_]: Async: Logger](
    state:    State[F],
    source:   HostId,
    response: NonEmptyChain[(BlockHeader, Either[BlockBodyOrTransactionError, UnverifiedBlockBody])]
  ): F[(State[F], Response[F])] = {
    val responseLog =
      response.map { case (header, res) => show"${header.id} with res ${res.isRight}" }.mkString_(", ")

    val successfullyDownloadedBodies =
      response.collect { case (header, Right(body)) => (header, body) }.toList

    for {
      _       <- Logger[F].info(show"Download next bodies: $responseLog")
      _       <- processBodyDownloadPerformance(state.peersManager, response)
      _       <- processBlockBodyDownloadErrors(state.peersManager, source, response)
      sentIds <- sendSuccessfulBodiesPrefix(state, successfullyDownloadedBodies)
      _       <- Logger[F].info(show"Send bodies prefix to block checker $sentIds")
      _ <- saveDownloadResultToCache(state.bodyRequests, successfullyDownloadedBodies.map { case (h, b) => (h.id, b) })
    } yield (state, state)
  }

  private def processBodyDownloadPerformance[F[_]: Async](
    peersManager: PeersManagerActor[F],
    response:     NonEmptyChain[(BlockHeader, Either[BlockBodyOrTransactionError, UnverifiedBlockBody])]
  ): F[Unit] =
    response
      .collect { case (_, Right(body)) => body }
      .traverse { h =>
        peersManager.sendNoWait(
          PeersManager.Message.DownloadTimeBody(h.source, h.downloadTimeMs, h.downloadTimeTxMs)
        )
      }
      .void

  private def processBlockBodyDownloadErrors[F[_]: Async](
    peersManager: PeersManagerActor[F],
    source:       HostId,
    response:     NonEmptyChain[(BlockHeader, Either[BlockBodyOrTransactionError, UnverifiedBlockBody])]
  ): F[Unit] = {
    val errorsOpt =
      NonEmptyChain.fromChain(response.collect { case (id, Left(error)) => (id, error) })

    errorsOpt match {
      case Some(errors) =>
        errors
          .forall(_._2.notCritical)
          .pure[F]
          .ifM(
            ifTrue = peersManager.sendNoWait(PeersManager.Message.NonCriticalErrorForHost(source)),
            ifFalse = peersManager.sendNoWait(PeersManager.Message.CriticalErrorForHost(source))
          ) >>
        peersManager.sendNoWait(PeersManager.Message.BlockBodyRequest(None, errors.map(_._1)))

      case None => ().pure[F]
    }
  }

  private def invalidateBlockId[F[_]: Async](
    state:           State[F],
    source:          HostId,
    @nowarn blockId: BlockId
  ): F[(State[F], Response[F])] =
    // TODO add cache for invalid block thus no longer accept blocks with that particular id
    state.peersManager.sendNoWait(PeersManager.Message.CriticalErrorForHost(source)) >>
    (state, state).pure[F]

  // Send to block checker first available body for checking,
  // i.e. successfully downloaded body with already stored parent.
  // We can't send whole response at once because some parents could be missing
  private def sendSuccessfulBodiesPrefix[F[_]: Async: Logger](
    state:                        State[F],
    successfullyDownloadedBodies: List[(BlockHeader, UnverifiedBlockBody)]
  ): F[Seq[BlockHeader]] = {
    val sendMessage =
      for {
        headersAndBodies   <- OptionT.fromOption[F](NonEmptyChain.fromSeq(successfullyDownloadedBodies))
        newHeaderAndBodies <- OptionT(dropKnownPrefix(headersAndBodies.toList, state.bodyStore)(_.id))
        (firstBlockHeader, firstBlockBody) = newHeaderAndBodies.head
        _       <- OptionT(state.bodyStore.get(firstBlockHeader.parentHeaderId))
        _       <- OptionT.liftF(Logger[F].debug(show"Parent of block body ${firstBlockHeader.id} is already adopted"))
        sentIds <- sendBodies(state, NonEmptyChain.one((firstBlockHeader, firstBlockBody)))
      } yield sentIds

    sendMessage.getOrElse(Seq.empty[BlockHeader])
  }

  private def sendHeaders[F[_]: Async: Logger](
    state:  State[F],
    toSend: NonEmptyChain[UnverifiedBlockHeader]
  ): OptionT[F, NonEmptyChain[BlockHeader]] =
    for {
      blockChecker <- OptionT
        .fromOption[F](state.blockCheckerOpt)
        .flatTapNone(Logger[F].error(show"Failed to find block checker in Requests Proxy"))
      _ <- OptionT.liftF(blockChecker.sendNoWait(BlockChecker.Message.RemoteBlockHeaders(toSend)))
    } yield toSend.map(_.blockHeader)

  private def sendBodies[F[_]: Async](
    state:  State[F],
    toSend: NonEmptyChain[(BlockHeader, UnverifiedBlockBody)]
  ): OptionT[F, Seq[BlockHeader]] =
    for {
      blockChecker <- OptionT.fromOption[F](state.blockCheckerOpt)
      _            <- OptionT.liftF(blockChecker.sendNoWait(BlockChecker.Message.RemoteBlockBodies(toSend)))
    } yield toSend.toList.map(_._1)

  private def getAvailablePrefix[F[_]: Async, T, I](
    requestsMap: CaffeineCache[F, BlockId, Option[T]],
    request:     NonEmptyChain[I]
  )(ItoId: I => BlockId): F[Seq[(I, T)]] =
    for {
      requests <- request.traverse(r => requestsMap.get(ItoId(r)).map(d => (r, d.flatten)))
    } yield requests.takeWhile_ { case (_, bodyOpt) => bodyOpt.isDefined }.map { case (i, bodyOpt) => (i, bodyOpt.get) }

  // request shall be expired, there is no strict guarantee that we will receive response on the request
  private val requestTTL = 5.seconds

  private def saveDownloadRequestToCache[F[_]: Async, T](
    cache: CaffeineCache[F, BlockId, Option[T]],
    ids:   Seq[BlockId]
  ) =
    ids.traverse(cache.put(_)(None, requestTTL.some))

  private def saveDownloadResultToCache[F[_]: Async, T](
    cache:      CaffeineCache[F, BlockId, Option[T]],
    idsAndData: Seq[(BlockId, T)]
  ) =
    idsAndData.traverse { case (id, data) => cache.put(id)(Option(data), None) }
}
