package co.topl.networking.blockchain

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Source}
import akka.util.ByteString
import cats.effect.Async
import cats.implicits._
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.models.{BlockBodyV2, BlockHeaderV2, Transaction, TypedIdentifier}
import co.topl.networking.TypedProtocolSetFactory.implicits._
import co.topl.networking._
import co.topl.networking.blockchain.BlockchainMultiplexerCodecs.longTypedIdentifierOptTransmittable
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

  private def createFactory[F[_]: Async: Logger: FToFuture](protocolServer: BlockchainPeerServer[F])(implicit
    materializer:                                                           Materializer
  ): TypedProtocolSetFactory[F, BlockchainPeerClient[F]] = {
    val blockAdoptionRecipF =
      TypedProtocolSetFactory.CommonProtocols.notificationReciprocated(
        BlockchainProtocols.BlockAdoption,
        protocolServer.localBlockAdoptions,
        1: Byte,
        2: Byte
      )

    val headerRecipF =
      TypedProtocolSetFactory.CommonProtocols.requestResponseReciprocated(
        BlockchainProtocols.Header,
        protocolServer.getLocalHeader,
        3: Byte,
        4: Byte
      )

    val bodyRecipF =
      TypedProtocolSetFactory.CommonProtocols.requestResponseReciprocated(
        BlockchainProtocols.Body,
        protocolServer.getLocalBody,
        5: Byte,
        6: Byte
      )

    val transactionRecipF =
      TypedProtocolSetFactory.CommonProtocols.requestResponseReciprocated(
        BlockchainProtocols.Transaction,
        protocolServer.getLocalTransaction,
        7: Byte,
        8: Byte
      )

    val idAtHeightRecipF =
      TypedProtocolSetFactory.CommonProtocols.requestResponseReciprocated(
        BlockchainProtocols.BlockIdAtHeight,
        { case (height, _) => protocolServer.getLocalBlockAtHeight(height) },
        9: Byte,
        10: Byte
      )

    (connectedPeer: ConnectedPeer, connectionLeader: ConnectionLeader) =>
      for {
        (adoptionTypedSubHandlers, remoteBlockIdsSource)           <- blockAdoptionRecipF.ap(connectionLeader.pure[F])
        (headerTypedSubHandlers, headerReceivedCallback)           <- headerRecipF.ap(connectionLeader.pure[F])
        (bodyTypedSubHandlers, bodyReceivedCallback)               <- bodyRecipF.ap(connectionLeader.pure[F])
        (transactionTypedSubHandlers, transactionReceivedCallback) <- transactionRecipF.ap(connectionLeader.pure[F])
        (idAtHeightTypedSubHandlers, heightIdReceivedCallback)     <- idAtHeightRecipF.ap(connectionLeader.pure[F])
        blockchainProtocolClient = new BlockchainPeerClient[F] {
          val remotePeerAdoptions: F[Source[TypedIdentifier, NotUsed]] = remoteBlockIdsSource.pure[F]
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
          adoptionTypedSubHandlers ++ headerTypedSubHandlers ++ bodyTypedSubHandlers ++ transactionTypedSubHandlers ++ idAtHeightTypedSubHandlers
      } yield subHandlers -> blockchainProtocolClient
  }

}
