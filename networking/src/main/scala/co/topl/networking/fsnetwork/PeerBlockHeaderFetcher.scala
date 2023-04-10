package co.topl.networking.fsnetwork

import cats.data.{NonEmptyChain, OptionT}
import cats.effect.kernel.{Async, Fiber}
import cats.effect.{Resource, Spawn}
import cats.implicits._
import cats.{Applicative, MonadThrow}
import co.topl.actor.{Actor, Fsm}
import co.topl.algebras.Store
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.{BlockHeader, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.networking.fsnetwork.BlockHeaderDownloadError.{HeaderHaveIncorrectId, UnknownError}
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
    Actor.make(initialState, getFsm[F])
  }

  private def startActor[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] = {
    require(state.fetchingFiber.isEmpty)

    for {
      _                 <- Logger[F].info(show"Start block header actor for host ${state.hostId}")
      newBlockIdsStream <- state.client.remotePeerAdoptions
      fiber             <- Spawn[F].start(slotDataFetcher(state, newBlockIdsStream).compile.drain)
      newState = state.copy(fetchingFiber = Option(fiber))
    } yield (newState, newState)
  }

  private def slotDataFetcher[F[_]: Async: Logger](
    state:             State[F],
    newBlockIdsStream: Stream[F, BlockId]
  ): Stream[F, Unit] =
    newBlockIdsStream.evalMap { blockId =>
      {
        for {
          _              <- OptionT.liftF(Logger[F].info(show"Got slot for $blockId from remote host ${state.hostId}"))
          newBlockId     <- isUnknownBlockOpt(state, blockId)
          remoteSlotData <- adoptRemoteSlotData(state, newBlockId)
          betterSlotData <- compareWithLocalChain(remoteSlotData, state)
          _              <- OptionT.liftF(sendProposalToBlockChecker(state, betterSlotData))
        } yield ()
      }.value.void
    }

  private def isUnknownBlockOpt[F[_]: Async: Logger](
    state:   State[F],
    blockId: BlockId
  ): OptionT[F, BlockId] =
    OptionT(
      state.slotDataStore
        .contains(blockId)
        .flatTap {
          case true  => Logger[F].info(show"Ignoring already-known block id=$blockId")
          case false => Logger[F].info(show"Received unknown block id=$blockId")
        }
        .map(Option.unless(_)(blockId))
    )

  private def adoptRemoteSlotData[F[_]: Async: Logger](
    state:   State[F],
    blockId: BlockId
  ): OptionT[F, NonEmptyChain[SlotData]] = {
    def adoptSlotData(slotData: SlotData) = {
      val slotBlockId = slotData.slotId.blockId
      val parentBlockId = slotData.parentSlotId.blockId

      Logger[F].info(show"Associating child=$slotBlockId to parent=$parentBlockId") >>
      state.blockIdTree.associate(slotBlockId, parentBlockId) >>
      Logger[F].info(show"Storing SlotData id=$slotBlockId") >>
      state.slotDataStore.put(slotBlockId, slotData)
    }

    for {
      tine <- buildTine(state.slotDataStore, state.client, blockId)
      _    <- OptionT.liftF(Logger[F].info(show"Retrieved remote tine length=${tine.length}"))
      _    <- OptionT.liftF(tine.traverse(adoptSlotData))
    } yield tine
  }

  // return: recent (current) block is the last
  private def buildTine[F[_]: Async: Logger](
    store:  Store[F, BlockId, SlotData],
    client: BlockchainPeerClient[F],
    from:   BlockId
  ): OptionT[F, NonEmptyChain[SlotData]] = {
    val getSlotData: BlockId => F[SlotData] =
      id =>
        store.get(id).flatMap {
          case Some(sd) => sd.pure[F]
          case None     => client.getRemoteSlotDataLogged(id)
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
          case true  => Logger[F].debug(show"Received header ${bestBlockInChain.slotId} is better than current block")
          case false => Logger[F].info(show"Ignoring weaker (or equal) block header id=${bestBlockInChain.slotId}")
        }
        .map(Option.when(_)(blockIds))
    )
  }

  private def sendProposalToBlockChecker[F[_]](state: State[F], slotData: NonEmptyChain[SlotData]) = {
    val message = BlockChecker.Message.RemoteSlotData(state.hostId, slotData)
    state.blockChecker.sendNoWait(message)
  }

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
        _                   <- Logger[F].info(show"Fetching remote header id=$blockId")
        (fetchedId, header) <- client.getRemoteHeaderOrError(blockId).map(h => (h.id, h))
        _                   <- Logger[F].info(show"Fetched remote header id=$blockId")
        _                   <- MonadThrow[F].raiseWhen(fetchedId =!= blockId)(HeaderHaveIncorrectId(blockId, fetchedId))
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
    val message: RequestsProxy.Message =
      RequestsProxy.Message.DownloadHeadersResponse(state.hostId, headersEither)
    state.requestsProxy.sendNoWait(message)
  }

  private def stopActor[F[_]: Applicative](state: State[F]): F[(State[F], Response[F])] = {
    state.fetchingFiber.map(_.cancel)
    val newState = state.copy(fetchingFiber = None)
    (newState, newState).pure[F]
  }

}
