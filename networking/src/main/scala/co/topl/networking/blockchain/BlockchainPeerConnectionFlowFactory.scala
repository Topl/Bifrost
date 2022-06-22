package co.topl.networking.blockchain

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Source}
import akka.util.ByteString
import cats.effect.Async
import cats.implicits._
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.models.{BlockBodyV2, BlockHeaderV2, SlotData, Transaction, TypedIdentifier}
import co.topl.networking.TypedProtocolSetFactory.implicits._
import co.topl.networking._
import co.topl.networking.blockchain.NetworkTypeTags._
import co.topl.networking.p2p.{ConnectedPeer, ConnectionLeader}
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

/**
 * Produces a function which accepts a (connectedPeer, connectionLeader) and emits an Akka Stream Flow.  The flow performs
 * the inbound and outbound communications to a specific peer.
 *
 * Specifically, the Flow runs a Multiplexer which serves several Blockchain Typed Protocols.  The Typed Protocols
 * are instantiated using the methods from the provided `BlockchainPeerServer`.
 */
object BlockchainPeerConnectionFlowFactory {

  def make[F[_]: Async: Logger: FToFuture](peerServer: BlockchainPeerServer[F])(implicit
    materializer:                                      Materializer
  ): (ConnectedPeer, ConnectionLeader) => F[Flow[ByteString, ByteString, BlockchainPeerClient[F]]] =
    createFactory(peerServer).multiplexed

  private[blockchain] def createFactory[F[_]: Async: Logger: FToFuture](protocolServer: BlockchainPeerServer[F])(
    implicit materializer:                                                              Materializer
  ): TypedProtocolSetFactory[F, BlockchainPeerClient[F]] = {
    val blockAdoptionRecipF =
      TypedProtocolSetFactory.CommonProtocols.notificationReciprocated(
        BlockchainProtocols.BlockAdoption,
        protocolServer.localBlockAdoptions,
        1: Byte,
        2: Byte
      )
    val transactionNotificationRecipF =
      TypedProtocolSetFactory.CommonProtocols.notificationReciprocated(
        BlockchainProtocols.TransactionBroadcasts,
        protocolServer.localTransactionNotifications,
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
        .requestResponseReciprocated[F, (Long, Option[TypedIdentifier]), TypedIdentifier](
          BlockchainProtocols.BlockIdAtHeight,
          t => protocolServer.getLocalBlockAtHeight(t._1),
          13: Byte,
          14: Byte
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
        blockchainProtocolClient = new BlockchainPeerClient[F] {
          val remotePeer: F[ConnectedPeer] = connectedPeer.pure[F]
          val remotePeerAdoptions: F[Source[TypedIdentifier, NotUsed]] = remoteBlockIdsSource.pure[F]
          val remoteTransactionNotifications: F[Source[TypedIdentifier, NotUsed]] = remoteTransactionIdsSource.pure[F]
          def getRemoteSlotData(id: TypedIdentifier): F[Option[SlotData]] = slotDataReceivedCallback(id)
          def getRemoteHeader(id: TypedIdentifier): F[Option[BlockHeaderV2]] = headerReceivedCallback(id)
          def getRemoteBody(id: TypedIdentifier): F[Option[BlockBodyV2]] = bodyReceivedCallback(id)
          def getRemoteTransaction(id: TypedIdentifier): F[Option[Transaction]] = transactionReceivedCallback(id)
          def getRemoteBlockIdAtHeight(
            height:       Long,
            localBlockId: Option[TypedIdentifier]
          ): F[Option[TypedIdentifier]] =
            heightIdReceivedCallback((height, localBlockId))
        }
        subHandlers =
          adoptionTypedSubHandlers ++
            transactionNotificationTypedSubHandlers ++
            headerTypedSubHandlers ++
            bodyTypedSubHandlers ++
            transactionTypedSubHandlers ++
            idAtHeightTypedSubHandlers
      } yield subHandlers -> blockchainProtocolClient
  }

}
