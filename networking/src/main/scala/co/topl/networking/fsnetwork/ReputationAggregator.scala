package co.topl.networking.fsnetwork

import cats.data.NonEmptyChain
import cats.effect.kernel.Fiber
import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.ReputationAggregator.Message._
import org.typelevel.log4cats.Logger

object ReputationAggregator {

  case class State[F[_]](
    peerManager: PeersManagerActor[F],
    // TODO clear reputation map if we receive no updates for some host long time
    performanceReputation:    Map[HostId, HostReputationValue],
    blockProvidingReputation: Map[HostId, HostReputationValue],
    noveltyReputation:        Map[HostId, Long],
    networkConfig:            P2PNetworkConfig,
    reputationUpdateFiber:    Option[Fiber[F, Throwable, Unit]]
  )

  type Response[F[_]] = State[F]

  sealed trait Message

  object Message {

    /**
     * Stop tracking reputation for particular host
     * @param hostId host to stop tracking
     */
    case class PeerIsCold(hostId: HostId) extends Message

    /**
     * PingPong message from remote peer, which allow us to measure performance reputation for remote host
     * without exchange any application level information
     * @param hostId remote peer
     * @param response ping pong message response
     */
    case class PingPongMessagePing(hostId: HostId, response: Either[NetworkQualityError, Long]) extends Message

    /**
     * Information about how long it takes to download block header from remote host,
     * allow us to update performance reputation
     * @param hostId remote peer
     * @param delay header download time
     */
    case class DownloadTimeHeader(hostId: HostId, delay: Long) extends Message

    /**
     * Information about how long it takes to download block body from remote host,
     * allow us to update performance reputation
     *
     * @param hostId remote peer
     * @param bodyDelay body download time
     * @param txDelays transactions download time
     */
    case class DownloadTimeBody(hostId: HostId, bodyDelay: Long, txDelays: Seq[Long]) extends Message

    /**
     * Notification about connection to the new remote peer had been established
     * @param hostIds remote peer
     */
    case class NewHotPeer(hostIds: NonEmptyChain[HostId]) extends Message

    /**
     * Block providing update for hosts. We could have mentioned the same host multiply time,
     * then each element just indicate different block sources
     *
     * @param blocksSources blocksSources._1 is corresponding host
     *                      blocksSources._2 how many sources for some particular block is known.
     *                      For example if we receive previously unknown block from remote peer SomeHost,
     *                      then we will receive message NonEmptyChain(SomeHost -> 1),
     *                      where "1" indicates we have only one known source for that block
     */
    case class BlockProvidingReputationUpdate(blocksSources: NonEmptyChain[(HostId, Long)]) extends Message

    /**
     * Remote peer provide us remote slot data with better height than our current local chain,
     * but whole slot data chain turned out to be worse of local chain because of density rule
     * @param hostId remote peer
     */
    case class BadKLookbackSlotData(hostId: HostId) extends Message

    /**
     * Remote peer provide to us incorrect, by some resons, block.
     * For example it could be block with incorrect transaction(s)
     * @param hostId remote peer
     */
    case class HostProvideIncorrectBlock(hostId: HostId) extends Message

    /**
     * Tick, which do actual update of time based reputation
     */
    case object ReputationUpdateTick extends Message

    /**
     * Send warm hosts reputation to PeerManager
     */
    case object UpdateWarmHosts extends Message
  }

  type ReputationAggregatorActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger]: Fsm[F, State[F], Message, Response[F]] =
    Fsm {
      case (state, NewHotPeer(hostIds)) => newHotPeer(state, hostIds)
      case (state, PeerIsCold(hostId))  => removeReputationForHost(state, hostId)

      case (state, PingPongMessagePing(hostId, pongResponse)) => pongMessageProcessing(state, hostId, pongResponse)
      case (state, DownloadTimeHeader(hostId, delay))         => headerDownloadTime(state, hostId, delay)
      case (state, DownloadTimeBody(hostId, delay, txDelays)) => blockDownloadTime(state, hostId, delay, txDelays)
      case (state, BlockProvidingReputationUpdate(data))      => blockProvidingReputationUpdate(state, data)

      case (state, BadKLookbackSlotData(hostId))      => badKLoopbackSlotData(state, hostId)
      case (state, HostProvideIncorrectBlock(hostId)) => incorrectBlockReceived(state, hostId)

      case (state, ReputationUpdateTick) => processReputationUpdateTick(state)
      case (state, UpdateWarmHosts)      => processUpdateWarmHosts(state)
    }

  def makeActor[F[_]: Async: Logger](
    peersManager:             PeersManagerActor[F],
    networkConfig:            P2PNetworkConfig,
    performanceReputation:    Map[HostId, HostReputationValue] = Map.empty[HostId, HostReputationValue],
    blockProvidingReputation: Map[HostId, HostReputationValue] = Map.empty[HostId, HostReputationValue],
    newnessReputation:        Map[HostId, Long] = Map.empty[HostId, Long]
  ): Resource[F, ReputationAggregatorActor[F]] = {
    require(networkConfig.slotDuration.toMillis > 0) // TODO shall be checked before?

    val initialState = ReputationAggregator.State[F](
      peersManager,
      performanceReputation,
      blockProvidingReputation,
      newnessReputation,
      networkConfig,
      None
    )

    Actor.make[F, State[F], Message, Response[F]](initialState, getFsm[F])
  }

  private def processReputationUpdateTick[F[_]: Async](state: State[F]): F[(State[F], Response[F])] = {
    val newNoveltyReputationMap =
      state.blockProvidingReputation.map { case (host, reputation) =>
        (host, reputation * state.networkConfig.blockNoveltyDecoy)
      }

    val newRemotePeerNoveltyReputationMap =
      state.noveltyReputation.map { case (host, reputation) => (host, Math.max(reputation - 1, 0)) }

    val newState = state.copy(
      blockProvidingReputation = newNoveltyReputationMap,
      noveltyReputation = newRemotePeerNoveltyReputationMap
    )

    state.peerManager.sendNoWait(
      PeersManager.Message.UpdatedReputation(
        newState.performanceReputation,
        newState.blockProvidingReputation,
        newState.noveltyReputation
      )
    ) >>
    (newState, newState).pure[F]
  }

  private def processUpdateWarmHosts[F[_]: Async](state: State[F]): F[(State[F], Response[F])] =
    state.peerManager.sendNoWait(PeersManager.Message.UpdateWarmHosts(state.performanceReputation)) >>
    (state, state).pure[F]

  private def removeReputationForHost[F[_]: Async: Logger](
    state:  State[F],
    hostId: HostId
  ): F[(State[F], Response[F])] = {
    val newState = state.copy(
      performanceReputation = state.performanceReputation - hostId,
      blockProvidingReputation = state.blockProvidingReputation - hostId,
      noveltyReputation = state.noveltyReputation - hostId
    )

    Logger[F].info(show"Remove reputation for host $hostId") >>
    (newState, newState).pure[F]
  }

  private def blockProvidingReputationUpdate[F[_]: Async](
    state: State[F],
    data:  NonEmptyChain[(HostId, Long)]
  ): F[(State[F], Response[F])] = {
    val blockProvidingUpdate: Map[HostId, HostReputationValue] =
      data.toList
        .groupMapReduce(_._1)(_._2)(Math.min)
        .map { case (source, knownSourceCount) =>
          val newReputation = knownSourcesToReputation(state.networkConfig, knownSourceCount)
          val oldReputation = state.blockProvidingReputation.getOrElse(source, 0.0)
          (source, Math.max(newReputation, oldReputation))
        }

    val newBlockProvidingReputation = state.blockProvidingReputation ++ blockProvidingUpdate
    val newState = state.copy(blockProvidingReputation = newBlockProvidingReputation)
    (newState, newState).pure[F]
  }

  private def pongMessageProcessing[F[_]: Async: Logger](
    state:    State[F],
    hostId:   HostId,
    response: Either[NetworkQualityError, Long]
  ): F[(State[F], Response[F])] =
    response match {
      case Right(value) =>
        val newReputation = delayToReputation(state.networkConfig, value)
        val newPerformanceReputation =
          state.performanceReputation + (hostId -> newReputation)
        val newState = state.copy(performanceReputation = newPerformanceReputation)
        Logger[F].info(show"Got pong message delay $value from remote host $hostId") >>
        (newState, newState).pure[F]

      case Left(error) =>
        Logger[F].error(show"Bad pong message: $error from host $hostId") >>
        state.peerManager.sendNoWait(PeersManager.Message.BanPeer(hostId)) >>
        (state, state).pure[F]
    }

  private def incorrectBlockReceived[F[_]: Async: Logger](
    state:  State[F],
    hostId: HostId
  ): F[(State[F], Response[F])] =
    Logger[F].error(show"Received incorrect block from host $hostId") >>
    state.peerManager.sendNoWait(PeersManager.Message.BanPeer(hostId)) >>
    (state, state).pure[F]

  private def headerDownloadTime[F[_]: Async: Logger](
    state:  State[F],
    hostId: HostId,
    delay:  Long
  ): F[(State[F], Response[F])] = {
    val newReputation = delayToReputation(state.networkConfig, delay)
    val oldReputation = state.performanceReputation.get(hostId)
    val updatedReputation = calculatePerformanceReputation(oldReputation, newReputation)

    val newPerformanceReputation: Map[HostId, HostReputationValue] =
      state.performanceReputation + (hostId -> updatedReputation)
    val newState = state.copy(performanceReputation = newPerformanceReputation)

    Logger[F].info(show"Received header download from host $hostId with delay $delay") >>
    (newState, newState).pure[F]
  }

  private def blockDownloadTime[F[_]: Async: Logger](
    state:    State[F],
    hostId:   HostId,
    delay:    Long,
    tsDelays: Seq[Long]
  ): F[(State[F], Response[F])] = {
    val maxDelay = (tsDelays :+ delay).max
    val newReputation = delayToReputation(state.networkConfig, maxDelay)
    val oldReputation = state.performanceReputation.get(hostId)
    val updatedReputation = calculatePerformanceReputation(oldReputation, newReputation)

    val newPerformanceReputation: Map[HostId, HostReputationValue] =
      state.performanceReputation + (hostId -> updatedReputation)
    val newState = state.copy(performanceReputation = newPerformanceReputation)

    Logger[F].debug(show"Received block download from host $hostId with max delay $delay") >>
    (newState, newState).pure[F]
  }

  private def newHotPeer[F[_]: Async: Logger](
    state:   State[F],
    hostIds: NonEmptyChain[HostId]
  ): F[(State[F], Response[F])] = {
    val noveltyInSlots = state.networkConfig.remotePeerNoveltyInSlots

    val newNoveltyReputation =
      state.noveltyReputation ++ hostIds.map(hostId => hostId -> noveltyInSlots).toList.toMap
    val newBlockProvidingReputation =
      state.blockProvidingReputation ++ hostIds.map(hostId => hostId -> 0.0).toList.toMap
    val newPerformanceReputation =
      state.performanceReputation ++ hostIds.map(hostId => hostId -> 0.0).toList.toMap

    val newState = state.copy(
      noveltyReputation = newNoveltyReputation,
      blockProvidingReputation = newBlockProvidingReputation,
      performanceReputation = newPerformanceReputation
    )

    Logger[F].info(s"Start tracking reputation for hosts $hostIds") >>
    (newState, newState).pure[F]
  }

  private def badKLoopbackSlotData[F[_]: Async](state: State[F], hostId: HostId): F[(State[F], Response[F])] = {
    // shall be enable after fix BN-1129
    val newBlockProvidingMap = state.blockProvidingReputation // + (hostId -> 0.0)
    val newState = state.copy(blockProvidingReputation = newBlockProvidingMap)
    (newState, newState).pure[F]
  }

  def delayToReputation(networkConfig: P2PNetworkConfig, delayInMs: Long): HostReputationValue = {
    val reputationReducing = delayInMs.toDouble / networkConfig.performanceReputationMaxDelay

    networkConfig.performanceReputationInitialValue - reputationReducing
  }

  def knownSourcesToReputation(networkConfig: P2PNetworkConfig, knownSources: Long): HostReputationValue = {
    val reputationReducing: HostReputationValue = (knownSources - 1) * networkConfig.blockNoveltyReputationStep
    Math.max(networkConfig.blockNoveltyInitialValue - reputationReducing, 0)
  }

  private def calculatePerformanceReputation(oldValueOpt: Option[Double], newValue: Double): HostReputationValue =
    oldValueOpt.map(oldValue => (oldValue * 2 + newValue) / 3.0).getOrElse(newValue)
}
