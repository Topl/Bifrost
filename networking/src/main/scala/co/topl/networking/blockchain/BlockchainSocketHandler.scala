package co.topl.networking.blockchain

import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.{BlockHeader, SlotData}
import co.topl.node.models.{BlockBody, CurrentKnownHostsReq, CurrentKnownHostsRes, PingMessage, PongMessage}
import co.topl.networking._
import co.topl.networking.blockchain.NetworkTypeTags._
import co.topl.networking.p2p.ConnectionLeader
import co.topl.networking.p2p.ConnectedPeer
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import fs2._

object BlockchainSocketHandler {

  /**
   * Consumes the given Socket by applying Blockchain-specific Multiplexed Typed Protocols to serve the given application
   * functions.
   * @param peerServerF a function which creates a BlockchainPeerServer for the given ConnectedPeer
   * @param useClient a function which consumes the BlockchainPeerClient to serve the application
   * @param peer The remote peer
   * @param leader The connection leader
   * @param reads the input stream
   * @param writes the output stream
   * @return a Resource which completes when all processing has completed
   */
  def make[F[_]: Async: Logger](
    peerServerF: ConnectedPeer => Resource[F, BlockchainPeerServerAlgebra[F]],
    useClient:   BlockchainPeerClient[F] => Resource[F, Unit]
  )(
    peer:   ConnectedPeer,
    leader: ConnectionLeader,
    reads:  Stream[F, Byte],
    writes: Pipe[F, Byte, Nothing]
  ): Resource[F, Unit] =
    peerServerF(peer)
      .map(server => createFactory(server))
      .flatMap(_.multiplexed(useClient)(peer, leader, reads, writes))

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

    val pingPongF =
      TypedProtocolSetFactory.CommonProtocols.requestResponseReciprocated[F, PingMessage, PongMessage](
        BlockchainProtocols.PingPong,
        protocolServer.getPong,
        19: Byte,
        20: Byte
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

          def getPongMessage(req: PingMessage): F[Option[PongMessage]] = pingMessageReceivedCallback(req)
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
            pingPongHandlers
      } yield subHandlers -> blockchainProtocolClient
  }

}
