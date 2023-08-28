package co.topl.networking.fsnetwork

import cats.data.{EitherT, NonEmptyChain, OptionT}
import cats.effect.{Async, Concurrent, Resource}
import cats.effect.implicits._
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.algebras.Store
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.algebras.{BlockHeaderToBodyValidationAlgebra, LocalChainAlgebra}
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.networking.KnownHostOps
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.PeerActor.Message._
import co.topl.networking.fsnetwork.PeerBlockBodyFetcher.PeerBlockBodyFetcherActor
import co.topl.networking.fsnetwork.PeerBlockHeaderFetcher.PeerBlockHeaderFetcherActor
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.node.models.{CurrentKnownHostsReq, PingMessage}
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import scala.util.Random

object PeerActor {
  sealed trait Message

  object Message {

    /**
     * Update peer state
     * @param networkLevel do we run network level data exchange like measuring metwork quality
     * @param applicationLevel do we run application level data exchange
     */
    case class UpdateState(networkLevel: Boolean, applicationLevel: Boolean) extends Message

    /**
     * Request to download block headers from peer, downloaded headers will be sent to block checker directly
     * @param blockIds headers block id to download
     */
    case class DownloadBlockHeaders(blockIds: NonEmptyChain[BlockId]) extends Message

    /**
     * Request to download block bodies from peer, downloaded bodies will be sent to block checker directly
     * @param blockData bodies block id to download
     */
    case class DownloadBlockBodies(blockData: NonEmptyChain[BlockHeader]) extends Message

    /**
     * Request current tip from remote peer
     */
    case object GetCurrentTip extends Message

    /**
     * Request network quality measure
     */
    case object GetNetworkQuality extends Message

    /**
     * Request to get remote host's hot connections
     */
    case class GetHotPeersFromPeer(maxHosts: Int) extends Message

    /**
     * Get peer server address
     */
    case object GetPeerServerAddress extends Message
  }

  case class State[F[_]](
    hostId:               HostId,
    client:               BlockchainPeerClient[F],
    reputationAggregator: ReputationAggregatorActor[F],
    blockHeaderActor:     PeerBlockHeaderFetcherActor[F],
    blockBodyActor:       PeerBlockBodyFetcherActor[F],
    peersManager:         PeersManagerActor[F],
    networkLevel:         Boolean,
    applicationLevel:     Boolean,
    genesisBlockId:       BlockId
  )

  type Response[F[_]] = State[F]
  type PeerActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Concurrent: Logger]: Fsm[F, State[F], Message, Response[F]] = Fsm {
    case (state, UpdateState(networkLevel, applicationLevel)) => updateState(state, networkLevel, applicationLevel)
    case (state, DownloadBlockHeaders(blockIds))              => downloadHeaders(state, blockIds)
    case (state, DownloadBlockBodies(blockHeaders))           => downloadBodies(state, blockHeaders)
    case (state, GetCurrentTip)                               => getCurrentTip(state)
    case (state, GetNetworkQuality)                           => getNetworkQuality(state)
    case (state, GetHotPeersFromPeer(maxHosts))               => getHotPeersOfCurrentPeer(state, maxHosts)
    case (state, GetPeerServerAddress)                        => getPeerServerAddress(state)
  }

  def makeActor[F[_]: Async: Logger](
    hostId:                 HostId,
    networkAlgebra:         NetworkAlgebra[F],
    client:                 BlockchainPeerClient[F],
    requestsProxy:          RequestsProxyActor[F],
    reputationAggregator:   ReputationAggregatorActor[F],
    peersManager:           PeersManagerActor[F],
    localChain:             LocalChainAlgebra[F],
    slotDataStore:          Store[F, BlockId, SlotData],
    transactionStore:       Store[F, TransactionId, IoTransaction],
    blockIdTree:            ParentChildTree[F, BlockId],
    headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F]
  ): Resource[F, PeerActor[F]] = {
    val initNetworkLevel = false
    val initAppLevel = false

    for {
      header <- networkAlgebra.makePeerHeaderFetcher(
        hostId,
        client,
        requestsProxy,
        localChain,
        slotDataStore,
        blockIdTree
      )
      body <- networkAlgebra.makePeerBodyFetcher(
        hostId,
        client,
        requestsProxy,
        transactionStore,
        headerToBodyValidation
      )
      genesisSlotData <- localChain.genesis.toResource
      initialState = State(
        hostId,
        client,
        reputationAggregator,
        header,
        body,
        peersManager,
        initNetworkLevel,
        initAppLevel,
        genesisSlotData.slotId.blockId
      )
      _ <- verifyGenesisAgreement(initialState).toResource
      actorName = s"Peer Actor for peer $hostId"
      actor <- Actor.makeWithFinalize(actorName, initialState, getFsm[F], finalizer[F])
    } yield actor
  }

  private def finalizer[F[_]: Concurrent: Logger](state: State[F]): F[Unit] =
    Logger[F].info(show"Finishing actor for peer ${state.hostId}") >>
    stopApplicationLevel(state) >>
    stopNetworkLevel(state)

  private def updateState[F[_]: Concurrent: Logger](
    state:               State[F],
    newNetworkLevel:     Boolean,
    newApplicationLevel: Boolean
  ): F[(State[F], Response[F])] = {
    val networkLevel: F[Unit] =
      (state.networkLevel != newNetworkLevel, newNetworkLevel) match {
        case (true, true)  => startNetworkLevel(state)
        case (true, false) => stopNetworkLevel(state)
        case (false, _)    => ().pure[F]
      }

    val applicationLevel: F[Unit] =
      (state.applicationLevel != newApplicationLevel, newApplicationLevel) match {
        case (true, true)  => startApplicationLevel(state)
        case (true, false) => stopApplicationLevel(state)
        case (false, _)    => ().pure[F]
      }

    val newState: State[F] = state.copy(applicationLevel = newApplicationLevel, networkLevel = newNetworkLevel)
    applicationLevel >> networkLevel >> (newState, newState).pure[F]
  }

  private def startApplicationLevel[F[_]: Concurrent: Logger](state: State[F]): F[Unit] =
    Logger[F].info(show"Application level is enabled for ${state.hostId}") >>
    state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.StartActor) >>
    state.blockBodyActor.sendNoWait(PeerBlockBodyFetcher.Message.StartActor)

  private def stopApplicationLevel[F[_]: Concurrent: Logger](state: State[F]): F[Unit] =
    Logger[F].info(show"Application level is disabled for ${state.hostId}") >>
    state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.StopActor) >>
    state.blockBodyActor.sendNoWait(PeerBlockBodyFetcher.Message.StopActor)

  private def startNetworkLevel[F[_]: Logger](state: State[F]): F[Unit] =
    Logger[F].info(show"Network level is started for ${state.hostId}")

  private def stopNetworkLevel[F[_]: Logger](state: State[F]): F[Unit] =
    Logger[F].info(show"Network level is stop for ${state.hostId}")

  private def downloadHeaders[F[_]: Concurrent](
    state:    State[F],
    blockIds: NonEmptyChain[BlockId]
  ): F[(State[F], Response[F])] =
    state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.DownloadBlockHeaders(blockIds)) >>
    (state, state).pure[F]

  private def downloadBodies[F[_]: Concurrent](
    state:     State[F],
    blockData: NonEmptyChain[BlockHeader]
  ): F[(State[F], Response[F])] =
    state.blockBodyActor.sendNoWait(PeerBlockBodyFetcher.Message.DownloadBlocks(blockData)) >>
    (state, state).pure[F]

  private def getCurrentTip[F[_]: Concurrent](state: State[F]): F[(State[F], Response[F])] =
    state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.GetCurrentTip) >>
    (state, state).pure[F]

  private def getHotPeersOfCurrentPeer[F[_]: Concurrent: Logger](
    state:    State[F],
    maxHosts: Int
  ): F[(State[F], Response[F])] = {
    for {
      _        <- OptionT.liftF(Logger[F].debug(s"Request $maxHosts neighbour(s) from ${state.hostId}"))
      response <- OptionT(state.client.getRemoteKnownHosts(CurrentKnownHostsReq(maxHosts)))
      hotPeers <- OptionT
        .fromOption[F](NonEmptyChain.fromSeq(response.hotHosts.map(_.asRemoteAddress)))
        .flatTapNone(Logger[F].info(s"Got no hot peers from ${state.hostId}"))
      _ <- OptionT.liftF(Logger[F].debug(s"Got hot peers $hotPeers from ${state.hostId}"))
      _ <- OptionT.liftF(state.peersManager.sendNoWait(PeersManager.Message.AddKnownPeers(hotPeers)))
    } yield (state, state)
  }.getOrElse((state, state))

  private def getPeerServerAddress[F[_]: Concurrent: Logger](state: State[F]): F[(State[F], Response[F])] = {
    val peer = state.hostId
    for {
      _              <- OptionT.liftF(Logger[F].info(s"Request server address from $peer"))
      peerServerPort <- OptionT(state.client.remotePeerServerPort)
      message = PeersManager.Message.RemotePeerServerPort(peer, peerServerPort)
      _ <- OptionT.liftF(state.peersManager.sendNoWait(message))
    } yield (state, state)
  }.getOrElse(state, state)

  private def getNetworkQuality[F[_]: Concurrent: Logger](state: State[F]): F[(State[F], Response[F])] =
    getPing(state).value.flatMap { res =>
      val message =
        ReputationAggregator.Message.PingPongMessagePing(state.hostId, res)
      Logger[F].info(show"From host ${state.hostId}: $message") >>
      state.reputationAggregator.sendNoWait(message)
    } >> (state, state).pure[F]

  private val incorrectPongMessage: NetworkQualityError = NetworkQualityError.IncorrectPongMessage: NetworkQualityError
  private val pingMessageSize = 1024 // 1024 hardcoded on protobuf level

  private def getPing[F[_]: Concurrent](state: State[F]): EitherT[F, NetworkQualityError, Long] =
    for {
      pingMessage <- EitherT.liftF(PingMessage(Random.nextString(pingMessageSize)).pure[F])
      before      <- EitherT.liftF(System.currentTimeMillis().pure[F])
      pongMessage <- EitherT.fromOptionF(state.client.getPongMessage(pingMessage), NetworkQualityError.NoPongMessage)
      after       <- EitherT.liftF(System.currentTimeMillis().pure[F])
      pongCorrect = pongMessage.pong.reverse == pingMessage.ping
      ping = after - before
      res <- EitherT.fromEither[F](Either.cond(pongCorrect, ping, incorrectPongMessage))
    } yield res

  /**
   * Ensure that the two peers agree on a genesis block.  If not, raise an excecption.
   */
  private def verifyGenesisAgreement[F[_]: Async](state: State[F]) =
    state.client
      .getRemoteBlockIdAtHeight(1, state.genesisBlockId.some)
      .flatMap(
        OptionT
          .fromOption[F](_)
          .getOrRaise(new IllegalStateException("Remote peer does not have a genesis block"))
          .flatMap(remoteGenesisId =>
            Async[F].raiseWhen(remoteGenesisId =!= state.genesisBlockId)(
              new IllegalStateException("Remote peer is using a different genesis block")
            )
          )
      )
}
