package co.topl.networking.blockchain

import cats.data.OptionT
import cats.effect.implicits._
import cats.effect.std.Queue
import cats.effect.{Async, Deferred, Resource}
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.models.Bytes
import co.topl.networking.multiplexer.{MultiplexedBuffer, MultiplexedReaderWriter}
import co.topl.networking.p2p.ConnectedPeer
import co.topl.node.models._
import fs2._
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.FiniteDuration
import scala.language.implicitConversions

class BlockchainSocketHandler[F[_]: Async: Logger](
  server:         BlockchainPeerServerAlgebra[F],
  portQueues:     BlockchainMultiplexedBuffers[F],
  readerWriter:   MultiplexedReaderWriter[F],
  cache:          PeerCache[F],
  connectedPeer:  ConnectedPeer,
  requestTimeout: FiniteDuration
) {

  def client: Stream[F, BlockchainPeerClient[F]] =
    Stream
      .eval(Deferred[F, Unit])
      .flatMap(deferred =>
        Stream(new BlockchainPeerClient[F] {

          override def remotePeer: ConnectedPeer = connectedPeer

          override def remotePeerAsServer: F[Option[KnownHost]] =
            writeRequest(BlockchainMultiplexerId.RemotePeerServerRequest, (), portQueues.remotePeerServer)

          override def remotePeerAdoptions: F[Stream[F, BlockId]] =
            Async[F].delay(Stream.fromQueueUnterminated(cache.remoteBlockAdoptions))

          override def remoteTransactionNotifications: F[Stream[F, TransactionId]] =
            Async[F].delay(Stream.fromQueueUnterminated(cache.remoteTransactionAdoptions))

          override def getRemoteBlockIdAtDepth(depth: Long): F[Option[BlockId]] =
            writeRequest(BlockchainMultiplexerId.BlockIdAtDepthRequest, depth, portQueues.blockIdAtDepth)

          override def getRemoteSlotData(id: BlockId): F[Option[SlotData]] =
            writeRequest(BlockchainMultiplexerId.SlotDataRequest, id, portQueues.slotData)

          override def getRemoteHeader(id: BlockId): F[Option[BlockHeader]] =
            writeRequest(BlockchainMultiplexerId.HeaderRequest, id, portQueues.headers)

          override def getRemoteBody(id: BlockId): F[Option[BlockBody]] =
            writeRequest(BlockchainMultiplexerId.BodyRequest, id, portQueues.bodies)

          override def getRemoteTransaction(id: TransactionId): F[Option[IoTransaction]] =
            writeRequest(BlockchainMultiplexerId.TransactionRequest, id, portQueues.transactions)

          override def getRemoteBlockIdAtHeight(height: Long): F[Option[BlockId]] =
            writeRequest(BlockchainMultiplexerId.BlockIdAtHeightRequest, height, portQueues.blockIdAtHeight)

          override def getRemoteKnownHosts(request: CurrentKnownHostsReq): F[Option[CurrentKnownHostsRes]] =
            writeRequest(BlockchainMultiplexerId.KnownHostsRequest, request, portQueues.knownHosts)

          override def getPongMessage(request: PingMessage): F[Option[PongMessage]] =
            writeRequest(BlockchainMultiplexerId.PingRequest, request, portQueues.pingPong)

          override def notifyAboutThisNetworkLevel(networkLevel: Boolean): F[Unit] =
            writeRequest(BlockchainMultiplexerId.AppLevelRequest, networkLevel, portQueues.appLevel)

          override def closeConnection(): F[Unit] = deferred.complete(()).void
        })
          .mergeHaltR(Stream.eval(deferred.get).drain)
      )
      .concurrently(background.drain)

  private def background: Stream[F, Unit] =
    readerStream.merge(portQueueStreams).merge(cacheStreams)

  private def readerStream =
    readerWriter.read
      .evalMap { case (port, bytes) =>
        bytes.byteAt(0) match {
          case 0 => processRequest(port, bytes.substring(1))
          case 1 => processResponse(port, bytes.substring(1))
          case _ =>
            Async[F]
              .raiseError[Unit](new IllegalArgumentException("Not RequestResponse"))
        }
      }

  private def portQueueStreams =
    Stream(
      portQueues.blockIdAtHeight.backgroundRequestProcessor(onPeerRequestedBlockIdAtHeight),
      portQueues.blockIdAtDepth.backgroundRequestProcessor(onPeerRequestedBlockIdAtDepth),
      portQueues.slotData.backgroundRequestProcessor(onPeerRequestedSlotData),
      portQueues.headers.backgroundRequestProcessor(onPeerRequestedHeader),
      portQueues.bodies.backgroundRequestProcessor(onPeerRequestedBody),
      portQueues.transactions.backgroundRequestProcessor(onPeerRequestedTransaction),
      portQueues.blockAdoptions.backgroundRequestProcessor(_ => onPeerRequestedBlockNotification()),
      portQueues.transactionAdoptions.backgroundRequestProcessor(_ => onPeerRequestedTransactionNotification()),
      portQueues.knownHosts.backgroundRequestProcessor(_ => onPeerRequestedKnownHosts()),
      portQueues.remotePeerServer.backgroundRequestProcessor(_ => onPeerRequestedRemotePeerServer()),
      portQueues.pingPong.backgroundRequestProcessor(onPeerRequestedPing),
      portQueues.appLevel.backgroundRequestProcessor(onPeerNotifiedAppLevel)
    ).parJoinUnbounded

  private def cacheStreams =
    Stream(
      Stream.force(server.localBlockAdoptions).enqueueUnterminated(cache.localBlockAdoptions).void,
      Stream
        .force(server.localTransactionNotifications)
        .enqueueUnterminated(cache.localTransactionAdoptions)
        .void,
      Stream
        .repeatEval(
          writeRequestNoTimeout(BlockchainMultiplexerId.BlockAdoptionRequest, (), portQueues.blockAdoptions)
        )
        .enqueueUnterminated(cache.remoteBlockAdoptions),
      Stream
        .repeatEval(
          writeRequestNoTimeout(
            BlockchainMultiplexerId.TransactionNotificationRequest,
            (),
            portQueues.transactionAdoptions
          )
        )
        .enqueueUnterminated(cache.remoteTransactionAdoptions)
    ).parJoinUnbounded

  private def processRequest(port: Int, data: Bytes): F[Unit] =
    OptionT
      .fromOption[F](BlockchainMultiplexerId.parse(port))
      .getOrRaise(new IllegalArgumentException(s"Invalid port=$port"))
      .flatMap {
        case BlockchainMultiplexerId.BlockIdAtHeightRequest =>
          portQueues.blockIdAtHeight.processRequest(tDecoded[Long](data))
        case BlockchainMultiplexerId.BlockIdAtDepthRequest =>
          portQueues.blockIdAtDepth.processRequest(tDecoded[Long](data))
        case BlockchainMultiplexerId.SlotDataRequest =>
          portQueues.slotData.processRequest(tDecoded[BlockId](data))
        case BlockchainMultiplexerId.HeaderRequest =>
          portQueues.headers.processRequest(tDecoded[BlockId](data))
        case BlockchainMultiplexerId.BodyRequest =>
          portQueues.bodies.processRequest(tDecoded[BlockId](data))
        case BlockchainMultiplexerId.BlockAdoptionRequest =>
          portQueues.blockAdoptions.processRequest(())
        case BlockchainMultiplexerId.TransactionRequest =>
          portQueues.transactions.processRequest(tDecoded[TransactionId](data))
        case BlockchainMultiplexerId.TransactionNotificationRequest =>
          portQueues.transactionAdoptions.processRequest(())
        case BlockchainMultiplexerId.KnownHostsRequest =>
          portQueues.knownHosts.processRequest(tDecoded[CurrentKnownHostsReq](data))
        case BlockchainMultiplexerId.RemotePeerServerRequest =>
          portQueues.remotePeerServer.processRequest(())
        case BlockchainMultiplexerId.PingRequest =>
          portQueues.pingPong.processRequest(tDecoded[PingMessage](data))
        case BlockchainMultiplexerId.AppLevelRequest =>
          portQueues.appLevel.processRequest(tDecoded[Boolean](data))
      }

  private def processResponse(port: Int, data: Bytes): F[Unit] =
    OptionT
      .fromOption[F](BlockchainMultiplexerId.parse(port))
      .getOrRaise(new IllegalArgumentException(s"Invalid port=$port"))
      .flatMap {
        case BlockchainMultiplexerId.BlockIdAtHeightRequest =>
          portQueues.blockIdAtHeight.processResponse(tDecoded[Option[BlockId]](data))
        case BlockchainMultiplexerId.BlockIdAtDepthRequest =>
          portQueues.blockIdAtDepth.processResponse(tDecoded[Option[BlockId]](data))
        case BlockchainMultiplexerId.SlotDataRequest =>
          portQueues.slotData.processResponse(tDecoded[Option[SlotData]](data))
        case BlockchainMultiplexerId.HeaderRequest =>
          portQueues.headers.processResponse(tDecoded[Option[BlockHeader]](data))
        case BlockchainMultiplexerId.BodyRequest =>
          portQueues.bodies.processResponse(tDecoded[Option[BlockBody]](data))
        case BlockchainMultiplexerId.BlockAdoptionRequest =>
          portQueues.blockAdoptions.processResponse(tDecoded[BlockId](data))
        case BlockchainMultiplexerId.TransactionRequest =>
          portQueues.transactions.processResponse(tDecoded[Option[IoTransaction]](data))
        case BlockchainMultiplexerId.TransactionNotificationRequest =>
          portQueues.transactionAdoptions.processResponse(tDecoded[TransactionId](data))
        case BlockchainMultiplexerId.KnownHostsRequest =>
          portQueues.knownHosts.processResponse(tDecoded[Option[CurrentKnownHostsRes]](data))
        case BlockchainMultiplexerId.RemotePeerServerRequest =>
          portQueues.remotePeerServer.processResponse(tDecoded[Option[KnownHost]](data))
        case BlockchainMultiplexerId.PingRequest =>
          portQueues.pingPong.processResponse(tDecoded[Option[PongMessage]](data))
        case BlockchainMultiplexerId.AppLevelRequest =>
          portQueues.appLevel.processResponse(())
      }

  private def writeRequest[Message: Transmittable, Response](
    port:    BlockchainMultiplexerId,
    message: Message,
    buffer:  MultiplexedBuffer[F, Message, Response]
  ): F[Response] =
    writeRequestNoTimeout(port, message, buffer).timeout(requestTimeout)

  private def writeRequestNoTimeout[Message: Transmittable, Response](
    port:    BlockchainMultiplexerId,
    message: Message,
    buffer:  MultiplexedBuffer[F, Message, Response]
  ): F[Response] =
    readerWriter.write(port.id, ZeroBS.concat(Transmittable[Message].transmittableBytes(message))) *>
    buffer.createResponse

  private def writeResponse[Message: Transmittable](
    port:    BlockchainMultiplexerId,
    message: Message
  ): F[Unit] =
    readerWriter.write(port.id, OneBS.concat(Transmittable[Message].transmittableBytes(message)))

  private def onPeerRequestedBlockIdAtHeight(height: Long) =
    server
      .getLocalBlockAtHeight(height)
      .flatMap(writeResponse(BlockchainMultiplexerId.BlockIdAtHeightRequest, _))

  private def onPeerRequestedBlockIdAtDepth(height: Long) =
    server
      .getLocalBlockAtDepth(height)
      .flatMap(writeResponse(BlockchainMultiplexerId.BlockIdAtDepthRequest, _))

  private def onPeerRequestedSlotData(id: BlockId) =
    server
      .getLocalSlotData(id)
      .flatMap(writeResponse(BlockchainMultiplexerId.SlotDataRequest, _))

  private def onPeerRequestedHeader(id: BlockId) =
    server
      .getLocalHeader(id)
      .flatMap(writeResponse(BlockchainMultiplexerId.HeaderRequest, _))

  private def onPeerRequestedBody(id: BlockId) =
    server
      .getLocalBody(id)
      .flatMap(writeResponse(BlockchainMultiplexerId.BodyRequest, _))

  private def onPeerRequestedTransaction(id: TransactionId) =
    server
      .getLocalTransaction(id)
      .flatMap(writeResponse(BlockchainMultiplexerId.TransactionRequest, _))

  private def onPeerRequestedKnownHosts() =
    server
      .getKnownHosts(CurrentKnownHostsReq())
      .flatMap(
        writeResponse(BlockchainMultiplexerId.KnownHostsRequest, _)
      )

  private def onPeerRequestedRemotePeerServer() =
    server.peerAsServer.flatMap(
      writeResponse(BlockchainMultiplexerId.RemotePeerServerRequest, _)
    )

  private def onPeerRequestedPing(message: PingMessage) =
    server
      .getPong(message)
      .flatMap(writeResponse(BlockchainMultiplexerId.PingRequest, _))

  private def onPeerRequestedBlockNotification() =
    cache.localBlockAdoptions.take
      .flatMap(writeResponse(BlockchainMultiplexerId.BlockAdoptionRequest, _))

  private def onPeerRequestedTransactionNotification() =
    cache.localTransactionAdoptions.take
      .flatMap(writeResponse(BlockchainMultiplexerId.TransactionNotificationRequest, _))

  private def onPeerNotifiedAppLevel(networkLevel: Boolean) =
    server.notifyApplicationLevel(networkLevel) >>
    writeResponse(BlockchainMultiplexerId.AppLevelRequest, ())

  implicit private def tDecoded[T: Transmittable](bytes: Bytes): T =
    Transmittable[T].fromTransmittableBytes(bytes) match {
      case Left(e)  => throw new IllegalArgumentException(e)
      case Right(v) => v
    }

}

object BlockchainSocketHandler {}

class PeerCache[F[_]](
  val localBlockAdoptions:        Queue[F, BlockId],
  val localTransactionAdoptions:  Queue[F, TransactionId],
  val remoteBlockAdoptions:       Queue[F, BlockId],
  val remoteTransactionAdoptions: Queue[F, TransactionId]
)

object PeerCache {

  def make[F[_]: Async]: Resource[F, PeerCache[F]] =
    (
      Queue.dropping[F, BlockId](256),
      Queue.dropping[F, TransactionId](256),
      Queue.dropping[F, BlockId](256),
      Queue.dropping[F, TransactionId](256)
    )
      .mapN(new PeerCache[F](_, _, _, _))
      .toResource
}
