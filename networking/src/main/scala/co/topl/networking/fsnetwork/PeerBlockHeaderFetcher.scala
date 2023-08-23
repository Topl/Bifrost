package co.topl.networking.fsnetwork

import cats.MonadThrow
import cats.data.{NonEmptyChain, OptionT}
import cats.effect.kernel.{Async, Fiber}
import cats.effect.{Resource, Spawn}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.algebras.Store
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.consensus.models.{BlockId, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockDownloadError.BlockHeaderDownloadError
import co.topl.networking.fsnetwork.BlockDownloadError.BlockHeaderDownloadError._
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.typeclasses.implicits._
import fs2.Stream
import org.typelevel.log4cats.Logger

object PeerBlockHeaderFetcher {
  sealed trait Message

  object Message {
    case object StartActor extends Message
    case object StopActor extends Message

    /**
     * Request to download block headers from peer, downloaded headers will be sent to block checker directly
     *
     * @param blockIds headers block id to download
     */
    case class DownloadBlockHeaders(blockIds: NonEmptyChain[BlockId]) extends Message

    /**
     * Get current tip from remote peer
     */
    case object GetCurrentTip extends Message
  }

  case class State[F[_]](
    hostId:        HostId,
    client:        BlockchainPeerClient[F],
    requestsProxy: RequestsProxyActor[F],
    localChain:    LocalChainAlgebra[F],
    slotDataStore: Store[F, BlockId, SlotData],
    blockIdTree:   ParentChildTree[F, BlockId],
    fetchingFiber: Option[Fiber[F, Throwable, Unit]]
  )

  type Response[F[_]] = State[F]
  type PeerBlockHeaderFetcherActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger]: Fsm[F, State[F], Message, Response[F]] = Fsm {
    case (state, Message.StartActor)               => startActor(state)
    case (state, Message.StopActor)                => stopActor(state)
    case (state, Message.DownloadBlockHeaders(id)) => downloadHeaders(state, id)
    case (state, Message.GetCurrentTip)            => getCurrentTip(state)
  }

  def makeActor[F[_]: Async: Logger](
    hostId:        HostId,
    client:        BlockchainPeerClient[F],
    requestsProxy: RequestsProxyActor[F],
    localChain:    LocalChainAlgebra[F],
    slotDataStore: Store[F, BlockId, SlotData],
    blockIdTree:   ParentChildTree[F, BlockId]
  ): Resource[F, Actor[F, Message, Response[F]]] = {
    val initialState =
      State(hostId, client, requestsProxy, localChain, slotDataStore, blockIdTree, None)
    val actorName = s"Header fetcher actor for peer $hostId"
    Actor.makeWithFinalize(actorName, initialState, getFsm[F], finalizer[F])
  }

  private def finalizer[F[_]: Async: Logger](state: State[F]): F[Unit] =
    stopActor(state).void

  private def startActor[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    if (state.fetchingFiber.isEmpty) {
      for {
        _                 <- Logger[F].info(show"Start block header actor for peer ${state.hostId}")
        newBlockIdsStream <- state.client.remotePeerAdoptions
        fiber             <- Spawn[F].start(slotDataFetcher(state, newBlockIdsStream).compile.drain)
        newState = state.copy(fetchingFiber = Option(fiber))
      } yield (newState, newState)
    } else {
      Logger[F].info(show"Ignore starting block header actor for peer ${state.hostId}") >>
      (state, state).pure[F]
    }

  private def slotDataFetcher[F[_]: Async: Logger](
    state:             State[F],
    newBlockIdsStream: Stream[F, BlockId]
  ): Stream[F, Unit] =
    // TODO close connection to remote peer in case of error
    newBlockIdsStream.evalMap { newBlockId =>
      getRemoteSlotDataByBlockId(state, newBlockId)
        .handleErrorWith(Logger[F].error(_)("Fetching slot data from remote host return error"))
    }

  private def getRemoteSlotDataByBlockId[F[_]: Async: Logger](state: State[F], blockId: BlockId): F[Unit] =
    for {
      _                 <- Logger[F].info(show"Got blockId: $blockId from remote peer ${state.hostId}")
      newSlotDataOpt    <- getRemoteBetterSlotDataChain(state, blockId).value
      _                 <- Logger[F].info(show"Build slot data chain from tip $blockId is ${newSlotDataOpt.isDefined}")
      newBlockSourceOpt <- buildBlockSource(state, blockId, newSlotDataOpt)
      _                 <- sendMessagesToProxy(state, newSlotDataOpt, newBlockSourceOpt)
    } yield ()

  private def getRemoteBetterSlotDataChain[F[_]: Async: Logger](
    state:   State[F],
    blockId: BlockId
  ): OptionT[F, NonEmptyChain[SlotData]] =
    for {
      newSlotData            <- OptionT.liftF(getSlotDataFromStorageOrRemote(state)(blockId))
      possibleBetterSlotData <- slotDataCouldBeBetter(state, newSlotData)
      savedSlotData          <- downloadAndSaveSlotDataChain(state, possibleBetterSlotData)
      betterSlotData         <- compareSlotDataWithLocal(savedSlotData, state)
    } yield betterSlotData

  private def buildBlockSource[F[_]: Async](
    state:          State[F],
    blockId:        BlockId,
    newSlotDataOpt: Option[NonEmptyChain[SlotData]]
  ): F[Option[NonEmptyChain[(HostId, BlockId)]]] =
    OptionT
      .fromOption[F](newSlotDataOpt)
      .map(newSlotData => newSlotData.map(sd => (state.hostId, sd.slotId.blockId)))
      .orElseF {
        sourceOfAlreadyAdoptedBlockIsUseful(state, blockId).ifM(
          ifTrue = Option(NonEmptyChain.one(state.hostId -> blockId)).pure[F],
          ifFalse = Option.empty[NonEmptyChain[(HostId, BlockId)]].pure[F]
        )
      }
      .value

  private def slotDataCouldBeBetter[F[_]: Async: Logger](state: State[F], newSlotData: SlotData): OptionT[F, SlotData] =
    OptionT
      .liftF(state.localChain.couldBeWorse(newSlotData).flatTap {
        case true =>
          Logger[F]
            .debug(show"Received tip ${newSlotData.slotId} from peer ${state.hostId}could be better than current block")
        case false =>
          Logger[F]
            .info(show"Ignoring weaker (or equal) block tip id=${newSlotData.slotId} from peer ${state.hostId}")
      })
      .flatMap(OptionT.when(_)(newSlotData))

  private def sendMessagesToProxy[F[_]: Async](
    state:          State[F],
    newSlotDataOpt: Option[NonEmptyChain[SlotData]],
    newSourcesOpt:  Option[NonEmptyChain[(HostId, BlockId)]]
  ): F[Unit] = {
    val newSourcesF =
      newSourcesOpt
        .map(sources => state.requestsProxy.sendNoWait(RequestsProxy.Message.BlocksSource(sources)))
        .getOrElse(().pure[F])

    val newSlotDataF =
      newSlotDataOpt
        .map(newSlotData =>
          state.requestsProxy.sendNoWait(RequestsProxy.Message.RemoteSlotData(state.hostId, newSlotData))
        )
        .getOrElse(().pure[F])

    newSourcesF >> newSlotDataF
  }

  // we still interesting in source if we receive current best block
  private def sourceOfAlreadyAdoptedBlockIsUseful[F[_]: Async](state: State[F], blockId: BlockId): F[Boolean] =
    state.localChain.head.map(_.slotId.blockId == blockId)

  private def downloadAndSaveSlotDataChain[F[_]: Async: Logger](
    state: State[F],
    from:  SlotData
  ): OptionT[F, NonEmptyChain[SlotData]] =
    for {
      slotDataChain <- buildSlotDataChain(state, from)
      _             <- OptionT.liftF(Logger[F].info(show"Going to save remote tine length=${slotDataChain.length}"))
      savedSlotData <- OptionT.liftF(saveSlotDataChain(state, slotDataChain))
    } yield savedSlotData

  private def saveSlotDataChain[F[_]: Async: Logger](
    state: State[F],
    tine:  NonEmptyChain[SlotData]
  ): F[NonEmptyChain[SlotData]] = {
    def adoptSlotData(slotData: SlotData) = {
      val slotBlockId = slotData.slotId.blockId
      val parentBlockId = slotData.parentSlotId.blockId

      Logger[F].info(show"Associating child=$slotBlockId to parent=$parentBlockId") >>
      state.blockIdTree.associate(slotBlockId, parentBlockId) >>
      Logger[F].info(show"Storing SlotData id=$slotBlockId") >>
      state.slotDataStore.put(slotBlockId, slotData)
    }

    tine.traverse(adoptSlotData) >> tine.pure[F]
  }

  private def getSlotDataFromStorageOrRemote[F[_]: Async: Logger](state: State[F])(blockId: BlockId): F[SlotData] =
    state.slotDataStore.get(blockId).flatMap {
      case Some(sd) => sd.pure[F]
      case None =>
        Logger[F].info(show"Fetching remote SlotData id=$blockId from peer ${state.hostId}") >>
        state.client.getSlotDataOrError(blockId, new NoSuchElementException(blockId.toString))
    }

  // return: recent (current) block is the last
  private def buildSlotDataChain[F[_]: Async: Logger](
    state: State[F],
    from:  SlotData
  ): OptionT[F, NonEmptyChain[SlotData]] = {
    val getSlotData: BlockId => F[SlotData] = id =>
      if (id == from.slotId.blockId) {
        from.pure[F]
      } else {
        getSlotDataFromStorageOrRemote(state)(id)
      }

    val tine = getFromChainUntil(
      (s: SlotData) => s.pure[F],
      getSlotData,
      (sd: SlotData) => state.slotDataStore.contains(sd.slotId.blockId)
    )(from.slotId.blockId)
      .handleErrorWith { error =>
        Logger[F].error(show"Failed to get remote slot data due to ${error.getLocalizedMessage}") >>
        List.empty[SlotData].pure[F] // TODO send information about error to reputation handler
      }

    OptionT(tine.map(NonEmptyChain.fromSeq(_)))
  }

  private def compareSlotDataWithLocal[F[_]: Async: Logger](
    slotData: NonEmptyChain[SlotData],
    state:    State[F]
  ): OptionT[F, NonEmptyChain[SlotData]] = {
    val bestSlotData = slotData.last
    val bestBlockId = bestSlotData.slotId.blockId
    OptionT(
      state.localChain
        .isWorseThan(bestSlotData)
        .flatMap {
          case true =>
            Logger[F].debug(show"Received tip $bestBlockId is better than current block") >>
            Option(slotData).pure[F]
          case false =>
            state.localChain
              .couldBeWorse(slotData.last)
              .ifM(
                ifTrue = Logger[F].info(show"Ignoring tip $bestBlockId because of density rule") >>
                  state.requestsProxy.sendNoWait(RequestsProxy.Message.BadKLookbackSlotData(state.hostId)),
                ifFalse =
                  Logger[F].info(show"Ignoring tip $bestBlockId because other better or equal block had been adopted")
              ) >>
            Option.empty[NonEmptyChain[SlotData]].pure[F]
        }
    )
  }

  private def getCurrentTip[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] = {
    for {
      _   <- OptionT.liftF(Logger[F].info(show"Requested current tip from peer ${state.hostId}"))
      tip <- OptionT(state.client.remoteCurrentTip())
      _   <- OptionT.liftF(getRemoteSlotDataByBlockId(state, tip))
      _   <- OptionT.liftF(Logger[F].info(show"Processed current tip $tip from peer ${state.hostId}"))
    } yield (state, state)
  }.getOrElse((state, state))
    .handleErrorWith(Logger[F].error(_)("Get tip from remote host return error") >> (state, state).pure[F])

  private def downloadHeaders[F[_]: Async: Logger](
    state:    State[F],
    blockIds: NonEmptyChain[BlockId]
  ): F[(State[F], Response[F])] =
    for {
      remoteHeaders <- getHeadersFromRemotePeer(state.client, state.hostId, blockIds)
      _             <- sendHeadersToProxy(state, NonEmptyChain.fromSeq(remoteHeaders).get)
    } yield (state, state)

  private def getHeadersFromRemotePeer[F[_]: Async: Logger](
    client:   BlockchainPeerClient[F],
    hostId:   HostId,
    blockIds: NonEmptyChain[BlockId]
  ): F[List[(BlockId, Either[BlockHeaderDownloadError, UnverifiedBlockHeader])]] =
    Stream
      .foldable[F, NonEmptyChain, BlockId](blockIds)
      .parEvalMapUnbounded(downloadHeader(client, hostId, _))
      .compile
      .toList

  private def downloadHeader[F[_]: Async: Logger](
    client:  BlockchainPeerClient[F],
    hostId:  HostId,
    blockId: BlockId
  ): F[(BlockId, Either[BlockHeaderDownloadError, UnverifiedBlockHeader])] = {
    val headerEither =
      for {
        _               <- Logger[F].info(show"Fetching remote header id=$blockId from peer $hostId")
        startHeaderTime <- System.currentTimeMillis().pure[F]
        headerWithNoId  <- client.getHeaderOrError(blockId, HeaderNotFoundInPeer)
        endHeaderTime   <- System.currentTimeMillis().pure[F]
        header          <- headerWithNoId.embedId.pure[F]
        _               <- Logger[F].info(show"Fetched remote header id=$blockId  from peer $hostId")
        _               <- MonadThrow[F].raiseWhen(header.id =!= blockId)(HeaderHaveIncorrectId(blockId, header.id))
      } yield UnverifiedBlockHeader(hostId, header, endHeaderTime - startHeaderTime)

    headerEither
      .map(blockHeader => Either.right[BlockHeaderDownloadError, UnverifiedBlockHeader](blockHeader))
      .handleError {
        case e: BlockHeaderDownloadError => Either.left[BlockHeaderDownloadError, UnverifiedBlockHeader](e)
        case unknownError                => Either.left(UnknownError(unknownError))
      }
      .flatTap {
        case Right(_) =>
          Logger[F].debug(show"Successfully download block header $blockId from peer $hostId")
        case Left(error) =>
          Logger[F].error(show"Failed download block $blockId from peer $hostId because of: ${error.toString}")
      }
      .map((blockId, _))
  }

  private def sendHeadersToProxy[F[_]](
    state:         State[F],
    headersEither: NonEmptyChain[(BlockId, Either[BlockHeaderDownloadError, UnverifiedBlockHeader])]
  ): F[Unit] = {
    val message: RequestsProxy.Message = RequestsProxy.Message.DownloadHeadersResponse(state.hostId, headersEither)
    state.requestsProxy.sendNoWait(message)
  }

  private def stopActor[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    state.fetchingFiber
      .map { fiber =>
        val newState = state.copy(fetchingFiber = None)
        Logger[F].info(show"Stop block header fetcher fiber for peer ${state.hostId}") >>
        fiber.cancel >>
        (newState, newState).pure[F]
      }
      .getOrElse {
        Logger[F].info(show"Ignoring stopping block header fetcher fiber for peer ${state.hostId}") >>
        (state, state).pure[F]
      }

}
