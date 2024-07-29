package co.topl.networking.legacy

import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.networking.blockchain.{BlockchainPeerClient, BlockchainPeerServerAlgebra}
import co.topl.networking.fsnetwork.P2PShowInstances._
import co.topl.networking.legacy.NetworkTypeTags._
import co.topl.networking.p2p.ConnectedPeer
import co.topl.node.models._
import co.topl.typeclasses.implicits._
import fs2._
import org.typelevel.log4cats.Logger

object LegacyBlockchainSocketHandler {

  /**
   * Consumes the given Socket by applying Blockchain-specific Multiplexed Typed Protocols to serve the given application
   * functions.
   * @param peerServerF a function which creates a BlockchainPeerServer for the given ConnectedPeer
   * @param useClientAndPeer a function which consumes the BlockchainPeerClient to serve the application
   * @param peer The remote peer
   * @param leader The connection leader
   * @param reads the input stream
   * @param writes the output stream
   * @return a Resource which completes when all processing has completed
   */
  def make[F[_]: Async: Logger](
    peerServerF:      ConnectedPeer => Resource[F, BlockchainPeerServerAlgebra[F]],
    useClientAndPeer: BlockchainPeerClient[F] => Resource[F, Unit]
  )(
    peer:   ConnectedPeer,
    leader: ConnectionLeader,
    reads:  Stream[F, Byte],
    writes: Pipe[F, Byte, Nothing],
    close:  F[Unit]
  ): Resource[F, Unit] =
    peerServerF(peer)
      .map(server => createFactory(server, close))
      .flatMap(_.multiplexed(useClientAndPeer)(peer, leader, reads, writes))

  private[legacy] def createFactory[F[_]: Async: Logger](
    protocolServer: BlockchainPeerServerAlgebra[F],
    close:          F[Unit]
  ): TypedProtocolSetFactory[F, BlockchainPeerClient[F]] = {
    val blockAdoptionRecipF =
      TypedProtocolSetFactory.CommonProtocols.notificationReciprocated(
        BlockchainProtocols.BlockAdoption,
        Stream.force(protocolServer.localBlockAdoptions),
        1: Byte,
        2: Byte
      )
    val transactionNotificationRecipF =
      TypedProtocolSetFactory.CommonProtocols.notificationReciprocated(
        BlockchainProtocols.TransactionBroadcasts,
        Stream.force(protocolServer.localTransactionNotifications),
        3: Byte,
        4: Byte
      )

    val slotDataRecipF =
      TypedProtocolSetFactory.CommonProtocols.requestResponseReciprocated(
        BlockchainProtocols.SlotData,
        protocolServer.getLocalSlotData,
        5: Byte,
        6: Byte
      )

    val headerRecipF =
      TypedProtocolSetFactory.CommonProtocols.requestResponseReciprocated(
        BlockchainProtocols.Header,
        protocolServer.getLocalHeader,
        7: Byte,
        8: Byte
      )

    val bodyRecipF =
      TypedProtocolSetFactory.CommonProtocols.requestResponseReciprocated(
        BlockchainProtocols.Body,
        protocolServer.getLocalBody,
        9: Byte,
        10: Byte
      )

    val transactionRecipF =
      TypedProtocolSetFactory.CommonProtocols.requestResponseReciprocated(
        BlockchainProtocols.Transaction,
        protocolServer.getLocalTransaction,
        11: Byte,
        12: Byte
      )

    val idAtHeightRecipF =
      TypedProtocolSetFactory.CommonProtocols
        .requestResponseReciprocated[F, (Long, Option[BlockId]), BlockId](
          BlockchainProtocols.BlockIdAtHeight,
          t => protocolServer.getLocalBlockAtHeight(t._1),
          13: Byte,
          14: Byte
        )

    val idAtDepthRecipF =
      TypedProtocolSetFactory.CommonProtocols
        .requestResponseReciprocated[F, Long, BlockId](
          BlockchainProtocols.BlockIdAtDepth,
          protocolServer.getLocalBlockAtDepth,
          15: Byte,
          16: Byte
        )

    val knownHostsRecipF =
      TypedProtocolSetFactory.CommonProtocols
        .requestResponseReciprocated[F, CurrentKnownHostsReq, CurrentKnownHostsRes](
          BlockchainProtocols.KnownHosts,
          protocolServer.getKnownHosts,
          17: Byte,
          18: Byte
        )

    val pingPongF =
      TypedProtocolSetFactory.CommonProtocols.requestResponseReciprocated[F, PingMessage, PongMessage](
        BlockchainProtocols.PingPong,
        protocolServer.getPong,
        19: Byte,
        20: Byte
      )

    val remotePeerServerF =
      TypedProtocolSetFactory.CommonProtocols.requestResponseReciprocated[F, Unit, KnownHost](
        BlockchainProtocols.RemotePeerServer,
        _ => protocolServer.peerAsServer,
        21: Byte,
        22: Byte
      )

    val notifyRemoteAppLevelF =
      TypedProtocolSetFactory.CommonProtocols.requestResponseReciprocated[F, Boolean, Unit](
        BlockchainProtocols.ApplicationLevelNotify,
        protocolServer.notifyApplicationLevel,
        23: Byte,
        24: Byte
      )

    (connectedPeer: ConnectedPeer, connectionLeader: ConnectionLeader) =>
      for {
        (adoptionTypedSubHandlers, remoteBlockIdsSource) <- blockAdoptionRecipF.ap(connectionLeader.pure[F])
        (transactionNotificationTypedSubHandlers, remoteTransactionIdsSource) <- transactionNotificationRecipF.ap(
          connectionLeader.pure[F]
        )
        (slotDataTypedSubHandlers, slotDataReceivedCallback)       <- slotDataRecipF.ap(connectionLeader.pure[F])
        (headerTypedSubHandlers, headerReceivedCallback)           <- headerRecipF.ap(connectionLeader.pure[F])
        (bodyTypedSubHandlers, bodyReceivedCallback)               <- bodyRecipF.ap(connectionLeader.pure[F])
        (transactionTypedSubHandlers, transactionReceivedCallback) <- transactionRecipF.ap(connectionLeader.pure[F])
        (idAtHeightTypedSubHandlers, heightIdReceivedCallback)     <- idAtHeightRecipF.ap(connectionLeader.pure[F])
        (idAtDepthTypedSubHandlers, depthIdReceivedCallback)       <- idAtDepthRecipF.ap(connectionLeader.pure[F])
        (knownHostsTypedSubHandlers, knownHostsReceivedCallback)   <- knownHostsRecipF.ap(connectionLeader.pure[F])
        (pingPongHandlers, pingMessageReceivedCallback)            <- pingPongF.ap(connectionLeader.pure[F])
        (peerServerHandlers, peerServerCallback)                   <- remotePeerServerF.ap(connectionLeader.pure[F])
        (appLevelNotifyHandlers, appNotifyCallback)                <- notifyRemoteAppLevelF.ap(connectionLeader.pure[F])
        blockchainProtocolClient = new BlockchainPeerClient[F] {
          val remotePeer: ConnectedPeer = connectedPeer
          val remotePeerAsServer: F[Option[KnownHost]] = peerServerCallback(())
          val remotePeerAdoptions: F[Stream[F, BlockId]] = remoteBlockIdsSource.pure[F]

          val remoteTransactionNotifications: F[Stream[F, TransactionId]] =
            remoteTransactionIdsSource.pure[F]

          def getRemoteSlotData(id: BlockId): F[Option[SlotData]] =
            slotDataReceivedCallback(id)

          def getRemoteHeader(id: BlockId): F[Option[BlockHeader]] =
            headerReceivedCallback(id)

          def getRemoteBody(id: BlockId): F[Option[BlockBody]] =
            bodyReceivedCallback(id)

          def getRemoteTransaction(id: TransactionId): F[Option[IoTransaction]] =
            transactionReceivedCallback(id)

          def getRemoteBlockIdAtHeight(height: Long): F[Option[BlockId]] =
            heightIdReceivedCallback((height, None)) // Send "None" for backwards-compatibility

          def getRemoteBlockIdAtDepth(depth: Long): F[Option[BlockId]] =
            depthIdReceivedCallback(depth)

          def getRemoteKnownHosts(req: CurrentKnownHostsReq): F[Option[CurrentKnownHostsRes]] =
            knownHostsReceivedCallback(req)

          def getPongMessage(req: PingMessage): F[Option[PongMessage]] = pingMessageReceivedCallback(req)

          def notifyAboutThisNetworkLevel(networkLevel: Boolean): F[Unit] =
            appNotifyCallback(networkLevel).void

          def closeConnection(): F[Unit] = close

          def getRemoteSlotDataWithParents(from: BlockId, to: BlockId): F[Option[List[SlotData]]] =
            getRemoteSlotData(to).map(_.map(List(_)))
        }

        subHandlers =
          adoptionTypedSubHandlers ++
            transactionNotificationTypedSubHandlers ++
            slotDataTypedSubHandlers ++
            headerTypedSubHandlers ++
            bodyTypedSubHandlers ++
            transactionTypedSubHandlers ++
            idAtHeightTypedSubHandlers ++
            idAtDepthTypedSubHandlers ++
            knownHostsTypedSubHandlers ++
            pingPongHandlers ++
            peerServerHandlers ++
            appLevelNotifyHandlers
      } yield subHandlers -> blockchainProtocolClient
  }

}
