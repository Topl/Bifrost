package co.topl.networking.fsnetwork

import cats.Applicative
import cats.data.NonEmptyChain
import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.algebras.Store
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.consensus.models.SlotData
import co.topl.eventtree.ParentChildTree
import co.topl.models.{Transaction, TypedIdentifier}
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.networking.fsnetwork.PeerActor.PeerActor
import co.topl.networking.fsnetwork.PeersManager.Message._
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import org.typelevel.log4cats.Logger

/**
 * Actor for managing peers
 */
object PeersManager {
  sealed trait Message

  object Message {

    /**
     * Setup appropriate actor for connection to peer specified by hostId, client shall be created outside
     * @param hostId host to connect
     * @param client client with already opened connection to host
     *               TODO client shall be created inside PeersManager
     * @tparam F effect
     */
    case class SetupPeer[F[_]](hostId: HostId, client: BlockchainPeerClient[F]) extends Message

    /**
     * Set block checker actor, can't be done in constructor due cyclic references
     * @param blockChecker block checker
     * @tparam F effect
     */
    case class SetupBlockChecker[F[_]](blockChecker: BlockCheckerActor[F]) extends Message

    /**
     * Set reputation aggregator actor, can't be done in constructor due cyclic references
     * @param aggregator reputation aggregator
     * @tparam F effect
     */
    case class SetupReputationAggregator[F[_]](aggregator: ReputationAggregatorActor[F]) extends Message

    /**
     * Update peer status due to some external event like changed reputation status
     * @param hostId peer to be affected
     * @param newState new state
     */
    case class UpdatePeerStatus(hostId: HostId, newState: PeerState) extends Message

    /**
     * @param hostId use hostId as hint for now, later we could have some kind of map of available peer -> headers
     * @param blockHeaders requested headers
     */
    case class BlockHeadersRequest(hostId: HostId, blockHeaders: NonEmptyChain[TypedIdentifier]) extends Message

    /**
     * @param hostId use hostId as hint for now, later we could have some kind of map of available peer -> blocks
     *               and send requests to many peers
     * @param blockBodies requested bodies
     */
    case class BlockDownloadRequest(hostId: HostId, blockBodies: NonEmptyChain[TypedIdentifier]) extends Message
  }

  // actor is option, because in future we shall be able to spawn actor without SetupPeer message,
  // thus actor could be shutdown in actor's non-active state
  case class Peer[F[_]: Logger](state: PeerState, actorOpt: Option[PeerActor[F]]) {

    def sendNoWait(message: PeerActor.Message): F[Unit] =
      actorOpt match {
        case Some(actor) => actor.sendNoWait(message)
        case None        => Logger[F].error(s"Try to send message $message to peer with no running client")
      }
  }

  case class State[F[_]](
    networkAlgebra:       NetworkAlgebra[F],
    reputationAggregator: Option[ReputationAggregatorActor[F]],
    blocksChecker:        Option[BlockCheckerActor[F]],
    peers:                Map[HostId, Peer[F]],
    localChain:           LocalChainAlgebra[F],
    slotDataStore:        Store[F, TypedIdentifier, SlotData],
    transactionStore:     Store[F, TypedIdentifier, Transaction],
    blockIdTree:          ParentChildTree[F, TypedIdentifier]
  )

  type Response[F[_]] = State[F]
  type PeersManagerActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger]: PeersManagerActor[F] => Fsm[F, State[F], Message, Response[F]] =
    (thisActor: PeersManagerActor[F]) =>
      Fsm {
        case (state, newPeer: SetupPeer[F] @unchecked) =>
          setupPeer(thisActor, state, newPeer)
        case (state, checker: SetupBlockChecker[F] @unchecked) =>
          setupBlockChecker(state, checker.blockChecker)
        case (state, aggregator: SetupReputationAggregator[F] @unchecked) =>
          setupRepAggregator(state, aggregator.aggregator)
        case (state, update: UpdatePeerStatus) =>
          updatePeerStatus(state, update)
        case (state, BlockHeadersRequest(hostId, blocks)) =>
          blockHeadersRequest(state, hostId, blocks)
        case (state, BlockDownloadRequest(hostId, blocks)) =>
          blockDownloadRequest(state, hostId, blocks)
      }

  def makeActor[F[_]: Async: Logger](
    networkAlgebra:   NetworkAlgebra[F],
    localChain:       LocalChainAlgebra[F],
    slotDataStore:    Store[F, TypedIdentifier, SlotData],
    transactionStore: Store[F, TypedIdentifier, Transaction],
    blockIdTree:      ParentChildTree[F, TypedIdentifier]
  ): Resource[F, PeersManagerActor[F]] = {
    val initialState =
      PeersManager.State[F](
        networkAlgebra,
        None,
        None,
        Map.empty[HostId, Peer[F]],
        localChain,
        slotDataStore,
        transactionStore,
        blockIdTree
      )
    Actor.makeFull(initialState, getFsm[F], finalizeActor[F])
  }

  private def setupPeer[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    setupPeer: SetupPeer[F]
  ): F[(State[F], Response[F])] = {
    val hostId: HostId = setupPeer.hostId
    val client: BlockchainPeerClient[F] = setupPeer.client

    require(state.blocksChecker.isDefined)
    require(state.reputationAggregator.isDefined)

    val peerActorF: F[PeerActor[F]] =
      thisActor.acquireActor(() =>
        PeerActor.makeActor(
          hostId,
          client,
          state.reputationAggregator.get,
          state.blocksChecker.get,
          state.localChain,
          state.slotDataStore,
          state.transactionStore,
          state.blockIdTree
        )
      )

    for {
      peer <- peerActorF.map(peerActor => Peer(PeerState.Cold, Option(peerActor)))
      newPeers = state.peers + (hostId -> peer)
      newState = state.copy(peers = newPeers)
    } yield (newState, newState)
  }

  private def setupBlockChecker[F[_]: Async: Logger](
    state:         State[F],
    blocksChecker: BlockCheckerActor[F]
  ): F[(State[F], Response[F])] = {
    val newState = state.copy(blocksChecker = Option(blocksChecker))
    Logger[F].info("Setup block checker") >>
    (newState, newState).pure[F]
  }

  private def setupRepAggregator[F[_]: Async: Logger](
    state:      State[F],
    aggregator: ReputationAggregatorActor[F]
  ): F[(State[F], Response[F])] = {
    val newState = state.copy(reputationAggregator = Option(aggregator))
    Logger[F].info("Setup reputation aggregation") >>
    (newState, newState).pure[F]
  }

  private def updatePeerStatus[F[_]: Applicative](
    state:     State[F],
    newStatus: UpdatePeerStatus
  ): F[(State[F], Response[F])] = {
    val hostId: HostId = newStatus.hostId

    val peer: Peer[F] = state.peers.getOrElse(hostId, createNewPeer(state, hostId))

    for {
      _ <- peer.sendNoWait(PeerActor.Message.UpdateState(newStatus.newState))
      newPeers = state.peers + (hostId -> peer)
      newState = state.copy(peers = newPeers)
    } yield (newState, newState)
  }

  private def createNewPeer[F[_]](state: State[F], hostId: HostId): Peer[F] = ???

  private def blockDownloadRequest[F[_]: Async](
    state:           State[F],
    requestedHostId: HostId,
    blocks:          NonEmptyChain[TypedIdentifier]
  ): F[(State[F], Response[F])] =
    state.peers.get(requestedHostId) match {
      case Some(peer) =>
        // TODO add logic if no peer actor is present
        peer.sendNoWait(PeerActor.Message.DownloadBlockBodies(blocks)) >>
        (state, state).pure[F]
      case None => ???
    }

  private def blockHeadersRequest[F[_]: Async](
    state:           State[F],
    requestedHostId: HostId,
    blocks:          NonEmptyChain[TypedIdentifier]
  ): F[(State[F], Response[F])] =
    state.peers.get(requestedHostId) match {
      case Some(peer) =>
        peer.sendNoWait(PeerActor.Message.DownloadBlockHeaders(blocks)) >>
        (state, state).pure[F]
      case None => ???
    }

  private def finalizeActor[F[_]: Applicative](currentState: State[F]): F[Unit] = ().pure[F]
}
