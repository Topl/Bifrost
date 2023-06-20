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
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
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
    blockChecker:  BlockCheckerActor[F],
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
    blockChecker:  BlockCheckerActor[F],
    requestsProxy: RequestsProxyActor[F],
    localChain:    LocalChainAlgebra[F],
    slotDataStore: Store[F, BlockId, SlotData],
    blockIdTree:   ParentChildTree[F, BlockId]
  ): Resource[F, Actor[F, Message, Response[F]]] = {
    val initialState = State(hostId, client, blockChecker, requestsProxy, localChain, slotDataStore, blockIdTree, None)
    Actor.makeWithFinalize(initialState, getFsm[F], finalizer[F])
  }

  private def finalizer[F[_]: Async: Logger](state: State[F]): F[Unit] =
    stopActor(state).void

  private def startActor[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    if (state.fetchingFiber.isEmpty) {
      for {
        _                 <- Logger[F].info(show"Start block header actor for host ${state.hostId}")
        newBlockIdsStream <- state.client.remotePeerAdoptions
        fiber             <- Spawn[F].start(slotDataFetcher(state, newBlockIdsStream).compile.drain)
        newState = state.copy(fetchingFiber = Option(fiber))
      } yield (newState, newState)
    } else {
      Logger[F].info(show"Ignore starting block header actor for host ${state.hostId}") >>
      (state, state).pure[F]
    }

  private def slotDataFetcher[F[_]: Async: Logger](
    state:             State[F],
    newBlockIdsStream: Stream[F, BlockId]
  ): Stream[F, Unit] =
    newBlockIdsStream.evalMap { blockId =>
      {
        for {
          _          <- OptionT.liftF(Logger[F].info(show"Got slot for $blockId from remote host ${state.hostId}"))
          newBlockId <- isUnknownBlockOpt(state, blockId)
          _          <- buildAndAdoptSlotDataForBlockId(state, newBlockId)
        } yield ()
      }.value.void.handleErrorWith(Logger[F].error(_)("Fetching slot data from remote host return error"))
    }

  private def buildAndAdoptSlotDataForBlockId[F[_]: Async: Logger](
    state:      State[F],
    newBlockId: BlockId
  ): OptionT[F, Unit] =
    for {
      slotDataChain  <- buildSlotDataChain(state.slotDataStore, state.client, newBlockId)
      _              <- OptionT.liftF(Logger[F].info(show"Retrieved remote tine length=${slotDataChain.length}"))
      savedSlotData  <- OptionT.liftF(saveSlotDataChain(state, slotDataChain))
      betterSlotData <- compareWithLocalChain(savedSlotData, state)
      _              <- OptionT.liftF(sendProposalToBlockChecker(state, betterSlotData))
    } yield ()

  private def isUnknownBlockOpt[F[_]: Async: Logger](state: State[F], blockId: BlockId): OptionT[F, BlockId] =
    OptionT(
      state.slotDataStore
        .contains(blockId)
        .flatTap {
          case true  => Logger[F].info(show"Ignoring already-known slot data for block id=$blockId")
          case false => Logger[F].info(show"Received unknown slot data for block id=$blockId")
        }
        .map(Option.unless(_)(blockId))
    )

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

  // return: recent (current) block is the last
  private def buildSlotDataChain[F[_]: Async: Logger](
    store:  Store[F, BlockId, SlotData],
    client: BlockchainPeerClient[F],
    from:   BlockId
  ): OptionT[F, NonEmptyChain[SlotData]] = {
    val getSlotData: BlockId => F[SlotData] =
      id =>
        store.get(id).flatMap {
          case Some(sd) => sd.pure[F]
          case None =>
            Logger[F].info(show"Fetching remote SlotData id=$id") >>
            client.getSlotDataOrError(id, new NoSuchElementException(id.toString))
        }

    val tine = getFromChainUntil(
      (s: SlotData) => s.pure[F],
      getSlotData,
      (sd: SlotData) => store.contains(sd.slotId.blockId)
    )(from)
      .handleErrorWith { error =>
        Logger[F].error(show"Failed to get remote slot data due to ${error.getLocalizedMessage}") >>
        List.empty[SlotData].pure[F] // TODO send information about error to reputation handler
      }

    OptionT(tine.map(NonEmptyChain.fromSeq(_)))
  }

  private def compareWithLocalChain[F[_]: Async: Logger](
    blockIds: NonEmptyChain[SlotData],
    state:    State[F]
  ): OptionT[F, NonEmptyChain[SlotData]] = {
    val bestBlockInChain = blockIds.last
    OptionT(
      state.localChain
        .isWorseThan(bestBlockInChain)
        .flatTap {
          case true  => Logger[F].debug(show"Received tip ${bestBlockInChain.slotId} is better than current block")
          case false => Logger[F].info(show"Ignoring weaker (or equal) block tip id=${bestBlockInChain.slotId}")
        }
        .map(Option.when(_)(blockIds))
    )
  }

  private def sendProposalToBlockChecker[F[_]](state: State[F], slotData: NonEmptyChain[SlotData]) = {
    val message = BlockChecker.Message.RemoteSlotData(state.hostId, slotData)
    state.blockChecker.sendNoWait(message)
  }

  private def getCurrentTip[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] = {
    for {
      _   <- OptionT.liftF(Logger[F].info(show"Requested current tip from host ${state.hostId}"))
      tip <- OptionT(state.client.remoteCurrentTip())
      _   <- buildAndAdoptSlotDataForBlockId(state, tip)
      _   <- OptionT.liftF(Logger[F].info(show"Send tip $tip from host ${state.hostId}"))
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
  ): F[List[(BlockId, Either[BlockHeaderDownloadError, BlockHeader])]] =
    Stream
      .foldable[F, NonEmptyChain, BlockId](blockIds)
      .parEvalMapUnbounded(downloadHeader(client, hostId, _))
      .compile
      .toList

  private def downloadHeader[F[_]: Async: Logger](
    client:  BlockchainPeerClient[F],
    hostId:  HostId,
    blockId: BlockId
  ): F[(BlockId, Either[BlockHeaderDownloadError, BlockHeader])] = {
    val headerEither =
      for {
        _      <- Logger[F].info(show"Fetching remote header id=$blockId")
        header <- client.getHeaderOrError(blockId, HeaderNotFoundInPeer).map(_.embedId)
        fetchedId = header.id
        _ <- Logger[F].info(show"Fetched remote header id=$blockId")
        _ <- MonadThrow[F].raiseWhen(fetchedId =!= blockId)(HeaderHaveIncorrectId(blockId, fetchedId))
      } yield header

    headerEither
      .map(blockHeader => Either.right[BlockHeaderDownloadError, BlockHeader](blockHeader))
      .handleError {
        case e: BlockHeaderDownloadError => Either.left[BlockHeaderDownloadError, BlockHeader](e)
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
    headersEither: NonEmptyChain[(BlockId, Either[BlockHeaderDownloadError, BlockHeader])]
  ): F[Unit] = {
    val message: RequestsProxy.Message = RequestsProxy.Message.DownloadHeadersResponse(state.hostId, headersEither)
    state.requestsProxy.sendNoWait(message)
  }

  private def stopActor[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    state.fetchingFiber
      .map { fiber =>
        val newState = state.copy(fetchingFiber = None)
        Logger[F].info(s"Stop block header fetcher fiber for host ${state.hostId}") >>
        fiber.cancel >>
        (newState, newState).pure[F]
      }
      .getOrElse {
        Logger[F].info(s"Ignoring stopping block header fetcher fiber for host ${state.hostId}") >>
        (state, state).pure[F]
      }

}
