package co.topl.networking.blockchain

import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.{BlockHeader, SlotData}
import co.topl.node.models.{BlockBody, CurrentKnownHostsReq, CurrentKnownHostsRes}
import co.topl.networking._
import co.topl.networking.blockchain.NetworkTypeTags._
import co.topl.networking.p2p.SocketLeader
import co.topl.networking.p2p.ConnectedPeer
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import fs2._
import fs2.io.net.Socket

/**
 * Produces a function which accepts a (connectedPeer, socketLeader) and emits an Akka Stream Flow.  The flow performs
 * the inbound and outbound communications to a specific peer.
 *
 * Specifically, the Flow runs a Multiplexer which serves several Blockchain Typed Protocols.  The Typed Protocols
 * are instantiated using the methods from the provided `BlockchainPeerServer`.
 */
object BlockchainSocketHandler {

  def make[F[_]: Async: Logger](
    peerServerF: ConnectedPeer => Resource[F, BlockchainPeerServerAlgebra[F]],
    useClient:   BlockchainPeerClient[F] => Resource[F, Unit]
  )(peer: ConnectedPeer, leader: SocketLeader, socket: Socket[F]): Resource[F, Unit] =
    peerServerF(peer)
      .map(server => createFactory(server))
      .flatMap(_.multiplexed(useClient)(peer, leader, socket))

  private[blockchain] def createFactory[F[_]: Async: Logger](
    protocolServer: BlockchainPeerServerAlgebra[F]
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

    (connectedPeer: ConnectedPeer, socketLeader: SocketLeader) =>
      for {
        (adoptionTypedSubHandlers, remoteBlockIdsSource) <- blockAdoptionRecipF.ap(socketLeader.pure[F])
        (transactionNotificationTypedSubHandlers, remoteTransactionIdsSource) <- transactionNotificationRecipF.ap(
          socketLeader.pure[F]
        )
        (slotDataTypedSubHandlers, slotDataReceivedCallback)       <- slotDataRecipF.ap(socketLeader.pure[F])
        (headerTypedSubHandlers, headerReceivedCallback)           <- headerRecipF.ap(socketLeader.pure[F])
        (bodyTypedSubHandlers, bodyReceivedCallback)               <- bodyRecipF.ap(socketLeader.pure[F])
        (transactionTypedSubHandlers, transactionReceivedCallback) <- transactionRecipF.ap(socketLeader.pure[F])
        (idAtHeightTypedSubHandlers, heightIdReceivedCallback)     <- idAtHeightRecipF.ap(socketLeader.pure[F])
        (idAtDepthTypedSubHandlers, depthIdReceivedCallback)       <- idAtDepthRecipF.ap(socketLeader.pure[F])
        (knownHostsTypedSubHandlers, knownHostsReceivedCallback)   <- knownHostsRecipF.ap(socketLeader.pure[F])
        blockchainProtocolClient = new BlockchainPeerClient[F] {
          val remotePeer: F[ConnectedPeer] = connectedPeer.pure[F]
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

          def getRemoteBlockIdAtHeight(height: Long, localBlockId: Option[BlockId]): F[Option[BlockId]] =
            heightIdReceivedCallback((height, localBlockId))

          def getRemoteBlockIdAtDepth(depth: Long): F[Option[BlockId]] =
            depthIdReceivedCallback(depth)

          def getRemoteKnownHosts(req: CurrentKnownHostsReq): F[Option[CurrentKnownHostsRes]] =
            knownHostsReceivedCallback(req)
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
            knownHostsTypedSubHandlers
      } yield subHandlers -> blockchainProtocolClient
  }

}
