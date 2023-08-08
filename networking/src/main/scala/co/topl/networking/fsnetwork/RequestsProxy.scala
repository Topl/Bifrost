package co.topl.networking.fsnetwork

import cats.data.{NonEmptyChain, OptionT}
import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.algebras.Store
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
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

    case object GetCurrentTips extends Message

    case class RemoteSlotData(hostId: HostId, slotData: NonEmptyChain[SlotData]) extends Message

    case class BlocksSource(sources: NonEmptyChain[(HostId, BlockId)]) extends Message

    case class BadKLookbackSlotData(hostId: HostId) extends Message

    // blockIds shall contains chain of linked blocks, for example if we have chain A -> B -> C
    // then A is parent of B and B is parent of C
    case class DownloadHeadersRequest(hostId: HostId, blockIds: NonEmptyChain[BlockId]) extends Message

    // response shall contains chain of linked blocks, for example if we have chain A -> B -> C
    // then A is parent of B and B is parent of C
    case class DownloadHeadersResponse(
      hostId:   HostId,
      response: NonEmptyChain[(BlockId, Either[BlockHeaderDownloadError, UnverifiedBlockHeader])]
    ) extends Message

    // blockIds shall contains chain of linked blocks, for example if we have chain A -> B -> C
    // then A is parent of B and B is parent of C
    case class DownloadBodiesRequest(hostId: HostId, blockHeaders: NonEmptyChain[BlockHeader]) extends Message

    // response shall contains chain of linked blocks, for example if we have chain A -> B -> C
    // then A is parent of B and B is parent of C
    case class DownloadBodiesResponse(
      source:   HostId,
      response: NonEmptyChain[(BlockHeader, Either[BlockBodyDownloadError, UnverifiedBlockBody])]
    ) extends Message

    // If verification of block is failed then we shall no longer process that particular blockId
    case class InvalidateBlockId(source: HostId, blockId: BlockId) extends Message
  }

  case class State[F[_]](
    reputationAggregator: ReputationAggregatorActor[F],
    peersManager:         PeersManagerActor[F],
    blockCheckerOpt:      Option[BlockCheckerActor[F]],
    headerStore:          Store[F, BlockId, BlockHeader],
    bodyStore:            Store[F, BlockId, BlockBody],
    headerRequests:       Cache[BlockId, Option[UnverifiedBlockHeader]],
    bodyRequests:         Cache[BlockId, Option[UnverifiedBlockBody]],
    blockSource:          Cache[BlockId, Set[HostId]]
  )

  type Response[F[_]] = State[F]
  type RequestsProxyActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger]: Fsm[F, State[F], Message, Response[F]] =
    Fsm {
      case (state, message: SetupBlockChecker[F] @unchecked)    => setupBlockChecker(state, message.blockCheckerActor)
      case (state, GetCurrentTips)                              => getCurrentTips(state)
      case (state, RemoteSlotData(hostId, slotData))            => processRemoteSlotData(state, hostId, slotData)
      case (state, BadKLookbackSlotData(hostId))                => resendBadKLookbackSlotData(state, hostId)
      case (state, BlocksSource(sources))                       => blocksSourceProcessing(state, sources)
      case (state, DownloadHeadersRequest(hostId, blockIds))    => downloadHeadersRequest(state, hostId, blockIds)
      case (state, DownloadHeadersResponse(source, response))   => downloadHeadersResponse(state, source, response)
      case (state, DownloadBodiesRequest(hostId, blockHeaders)) => downloadBodiesRequest(state, hostId, blockHeaders)
      case (state, DownloadBodiesResponse(source, response))    => downloadBodiesResponse(state, source, response)
      case (state, InvalidateBlockId(source, blockId))          => invalidateBlockId(state, source, blockId)
    }

  def makeActor[F[_]: Async: Logger](
    reputationAggregator: ReputationAggregatorActor[F],
    peersManager:         PeersManagerActor[F],
    headerStore:          Store[F, BlockId, BlockHeader],
    bodyStore:            Store[F, BlockId, BlockBody],
    headerRequests: Cache[BlockId, Option[UnverifiedBlockHeader]] =
      Caffeine.newBuilder.maximumSize(requestCacheSize).build[BlockId, Option[UnverifiedBlockHeader]](),
    bodyRequests: Cache[BlockId, Option[UnverifiedBlockBody]] =
      Caffeine.newBuilder.maximumSize(requestCacheSize).build[BlockId, Option[UnverifiedBlockBody]](),
    blockSource: Cache[BlockId, Set[HostId]] =
      Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
  ): Resource[F, RequestsProxyActor[F]] = {
    val initialState =
      State(
        reputationAggregator,
        peersManager,
        blockCheckerOpt = None,
        headerStore,
        bodyStore,
        headerRequests,
        bodyRequests,
        blockSource
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

  private def getCurrentTips[F[_]: Async](state: State[F]): F[(State[F], Response[F])] =
    state.peersManager.sendNoWait(PeersManager.Message.GetCurrentTips) >>
    (state, state).pure[F]

  private def processRemoteSlotData[F[_]: Async](
    state:    State[F],
    source:   HostId,
    slotData: NonEmptyChain[SlotData]
  ): F[(State[F], Response[F])] = {
    val message = BlockChecker.Message.RemoteSlotData(source, slotData)
    state.blockCheckerOpt.map(blockChecker => blockChecker.sendNoWait(message)).getOrElse(().pure[F]) >>
    (state, state).pure[F]
  }

  private def resendBadKLookbackSlotData[F[_]: Async: Logger](
    state:  State[F],
    source: HostId
  ): F[(State[F], Response[F])] =
    Logger[F].warn(show"Received declined remote slot data chain because of low density") >>
    state.reputationAggregator.sendNoWait(ReputationAggregator.Message.BadKLookbackSlotData(source)) >>
    (state, state).pure[F]

  private def blocksSourceProcessing[F[_]: Async](
    state:   State[F],
    sources: NonEmptyChain[(HostId, BlockId)]
  ): F[(State[F], Response[F])] = {
    val toSend: Seq[(HostId, Long)] =
      sources.toList.flatMap { case (host, blockId) =>
        val previousSource: Set[HostId] = state.blockSource.get(blockId).getOrElse(Set.empty[HostId])
        if (previousSource.contains(host)) {
          Option.empty[(HostId, Long)]
        } else {
          val newSource: Set[HostId] = previousSource + host
          state.blockSource.put(blockId, newSource)
          Option((host, newSource.size))
        }
      }

    val sendMessage: NonEmptyChain[(HostId, Long)] => F[Unit] =
      d => state.reputationAggregator.sendNoWait(ReputationAggregator.Message.BlockProvidingReputationUpdate(d))

    NonEmptyChain.fromSeq(toSend).map(sendMessage).getOrElse(().pure[F]) >> (state, state).pure[F]
  }

  private def downloadHeadersRequest[F[_]: Async: Logger](
    state:    State[F],
    hostId:   HostId,
    blockIds: NonEmptyChain[BlockId]
  ): F[(State[F], Response[F])] =
    for {
      _              <- Logger[F].info(show"Get request for headers downloads $blockIds")
      idsDownloaded  <- sendAlreadyDownloadedHeadersToBlockChecker(state, blockIds)
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
    blockIds: NonEmptyChain[BlockId]
  ): F[Seq[BlockId]] = {
    val availablePrefix = getAvailablePrefix(state.headerRequests, blockIds)(identity)

    val sentIdsOpt =
      for {
        newIdAndHeader <- OptionT(dropKnownPrefix(availablePrefix, state.headerStore)(identity))
        sentIds        <- sendHeaders(state, newIdAndHeader.map(_._2))
      } yield sentIds.toList.map(_.id)

    sentIdsOpt.getOrElse(Seq.empty)
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
    source:   HostId,
    response: NonEmptyChain[(BlockId, Either[BlockHeaderDownloadError, UnverifiedBlockHeader])]
  ): F[(State[F], Response[F])] = {
    val responseLog =
      response.map { case (id, res) => show"$id with res ${res.isRight}" }.mkString_(", ")

    val successfullyDownloadedHeaders =
      response.collect { case (id, Right(header)) => (id, header) }.toList

    for {
      _       <- Logger[F].info(show"Download next headers: $responseLog")
      _       <- processHeaderDownloadPerformance(state.reputationAggregator, response)
      _       <- processHeaderDownloadErrors(state.reputationAggregator, state.peersManager, source, response)
      sentIds <- sendSuccessfulHeadersPrefix(state, successfullyDownloadedHeaders)
      _       <- Logger[F].info(show"Send headers prefix to block checker $sentIds")
      _ = saveDownloadResultToCache(state.headerRequests, successfullyDownloadedHeaders)
    } yield (state, state)
  }

  private def processHeaderDownloadPerformance[F[_]: Async](
    reputationAggregator: ReputationAggregatorActor[F],
    response:             NonEmptyChain[(BlockId, Either[BlockHeaderDownloadError, UnverifiedBlockHeader])]
  ): F[Unit] =
    response
      .collect { case (_, Right(header)) => header }
      .traverse { h =>
        reputationAggregator.sendNoWait(ReputationAggregator.Message.DownloadTimeHeader(h.source, h.downloadTimeMs))
      }
      .void

  private def processHeaderDownloadErrors[F[_]: Async](
    reputationAggregator: ReputationAggregatorActor[F],
    peersManager:         PeersManagerActor[F],
    source:               HostId,
    response:             NonEmptyChain[(BlockId, Either[BlockHeaderDownloadError, UnverifiedBlockHeader])]
  ): F[Unit] = {
    val errorsOpt =
      NonEmptyChain.fromChain(response.collect { case (id, Left(error)) => (id, error) })

    errorsOpt match {
      case Some(errors) =>
        val reputationMessage: ReputationAggregator.Message =
          ReputationAggregator.Message.HostProvideIncorrectBlock(source)

        errors.traverse { case (id, _) =>
          reputationAggregator.sendNoWait(reputationMessage) >>
          // TODO we shall not try to download header from the same host, peer manager shall decide it
          peersManager.sendNoWait(PeersManager.Message.BlockHeadersRequest(source, NonEmptyChain.one(id)))
        }.void

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
      _ = saveDownloadRequestToCache(state.bodyRequests, forDownload)
    } yield (state, state)
  }

  // response with longest possible prefix for requested ids
  // where corresponding block body is already downloaded for each block id
  // also drop any data which is already in body storage
  private def sendAlreadyDownloadedBodiesToBlockChecker[F[_]: Async](
    state:    State[F],
    blockIds: NonEmptyChain[BlockHeader]
  ): F[Seq[BlockId]] = {
    val dataToSend = getAvailablePrefix(state.bodyRequests, blockIds)(_.id)

    val sentIdsOpt =
      for {
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
    val newBlockData = blockData.filterNot(d => state.bodyRequests.contains(d.id))
    NonEmptyChain
      .fromChain(newBlockData)
      .map { blockData =>
        state.peersManager.sendNoWait(PeersManager.Message.BlockBodyRequest(hostId, blockData)) >>
        blockData.map(_.id).toList.pure[F]
      }
      .getOrElse(List.empty[BlockId].pure[F])
  }

  private def downloadBodiesResponse[F[_]: Async: Logger](
    state:    State[F],
    source:   HostId,
    response: NonEmptyChain[(BlockHeader, Either[BlockBodyDownloadError, UnverifiedBlockBody])]
  ): F[(State[F], Response[F])] = {
    val responseLog =
      response.map { case (header, res) => show"${header.id} with res ${res.isRight}" }.mkString_(", ")

    val successfullyDownloadedBodies =
      response.collect { case (header, Right(body)) => (header, body) }.toList

    for {
      _       <- Logger[F].info(show"Download next bodies: $responseLog")
      _       <- processBodyDownloadPerformance(state.reputationAggregator, response)
      _       <- processBlockBodyDownloadErrors(state, source, response)
      sentIds <- sendSuccessfulBodiesPrefix(state, successfullyDownloadedBodies)
      _       <- Logger[F].info(show"Send bodies prefix to block checker $sentIds")
      _ = saveDownloadResultToCache(state.bodyRequests, successfullyDownloadedBodies.map { case (h, b) => (h.id, b) })
    } yield (state, state)
  }

  private def processBodyDownloadPerformance[F[_]: Async](
    reputationAggregator: ReputationAggregatorActor[F],
    response:             NonEmptyChain[(BlockHeader, Either[BlockBodyDownloadError, UnverifiedBlockBody])]
  ): F[Unit] =
    response
      .collect { case (_, Right(body)) => body }
      .traverse { h =>
        reputationAggregator.sendNoWait(
          ReputationAggregator.Message.DownloadTimeBody(h.source, h.downloadTimeMs, h.downloadTimeTxMs)
        )
      }
      .void

  private def processBlockBodyDownloadErrors[F[_]: Async](
    state:    State[F],
    source:   HostId,
    response: NonEmptyChain[(BlockHeader, Either[BlockBodyDownloadError, UnverifiedBlockBody])]
  ): F[Unit] = {
    val reputationAggregator: ReputationAggregatorActor[F] = state.reputationAggregator
    val peersManager: PeersManagerActor[F] = state.peersManager

    val errorsOpt =
      NonEmptyChain.fromChain(response.collect { case (header, Left(error)) => (header, error) })

    def processError(header: BlockHeader): F[Unit] = {
      val reputationMessage: ReputationAggregator.Message =
        ReputationAggregator.Message.HostProvideIncorrectBlock(source)

      {
        for {
          _ <- OptionT.liftF(reputationAggregator.sendNoWait(reputationMessage))
          message = PeersManager.Message.BlockBodyRequest(source, NonEmptyChain.one(header))
          _ <- OptionT.liftF(peersManager.sendNoWait(message))
        } yield ()
      }.getOrElse(())
    }

    errorsOpt match {
      case Some(errors) => errors.traverse { case (header, _) => processError(header) }.void
      case None         => ().pure[F]
    }
  }

  private def invalidateBlockId[F[_]: Async](
    state:   State[F],
    source:  HostId,
    blockId: BlockId
  ): F[(State[F], Response[F])] =
    // TODO add cache for invalid block thus no longer accept blocks with that particular id
    state.reputationAggregator.sendNoWait(ReputationAggregator.Message.HostProvideIncorrectBlock(source)) >>
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

  private def sendHeaders[F[_]: Async](
    state:  State[F],
    toSend: NonEmptyChain[UnverifiedBlockHeader]
  ): OptionT[F, NonEmptyChain[BlockHeader]] =
    for {
      blockChecker <- OptionT.fromOption[F](state.blockCheckerOpt)
      _            <- OptionT.liftF(blockChecker.sendNoWait(BlockChecker.Message.RemoteBlockHeaders(toSend)))
    } yield toSend.map(_.blockHeader)

  private def sendBodies[F[_]: Async](
    state:  State[F],
    toSend: NonEmptyChain[(BlockHeader, UnverifiedBlockBody)]
  ): OptionT[F, Seq[BlockHeader]] =
    for {
      blockChecker <- OptionT.fromOption[F](state.blockCheckerOpt)
      _            <- OptionT.liftF(blockChecker.sendNoWait(BlockChecker.Message.RemoteBlockBodies(toSend)))
    } yield toSend.toList.map(_._1)

  private def getAvailablePrefix[T, I](requestsMap: Cache[BlockId, Option[T]], request: NonEmptyChain[I])(
    ItoId: I => BlockId
  ): Seq[(I, T)] =
    request
      .map(id => (id, requestsMap.get(ItoId(id)).flatten))
      .takeWhile_ { case (_, bodyOpt) => bodyOpt.isDefined }
      .map { case (i, bodyOpt) => (i, bodyOpt.get) }

  private def saveDownloadRequestToCache[T](cache: Cache[BlockId, Option[T]], ids: Seq[BlockId]) =
    ids.map(id => cache.put(id, None))

  private def saveDownloadResultToCache[T](cache: Cache[BlockId, Option[T]], ids: Seq[(BlockId, T)]) =
    ids.map { case (id, data) => cache.put(id, Option(data)) }
}
