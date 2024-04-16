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

import scala.concurrent.duration.FiniteDuration
import scala.language.implicitConversions

/**
 * Provides a blockchain-based abstraction over a multiplexed P2P connection
 * @param server an implementation from the local application that can _provide_ data to the remote peer when requested
 * @param multiplexerBuffers Stores inbound and outbound request/response information for each multiplexer port
 * @param readerWriter an abstraction over a Socket, allowing for reading and writing multiplexer frame data
 * @param cache a cache/buffer for stream-based information, like block notifications
 * @param connectedPeer the remote peer
 * @param requestTimeout a timeout to apply to all requests
 */
class BlockchainSocketHandler[F[_]: Async](
  server:             BlockchainPeerServerAlgebra[F],
  multiplexerBuffers: BlockchainMultiplexedBuffers[F],
  readerWriter:       MultiplexedReaderWriter[F],
  cache:              PeerStreamBuffer[F],
  connectedPeer:      ConnectedPeer,
  requestTimeout:     FiniteDuration
) {

  /**
   * Returns a Stream containing a single BlockchainPeerClient that can be consumed by the application
   *
   * Running this Stream also runs background processing for this socket, including serving local data to the remote peer.
   *
   * Completion of this Stream will also shutdown the background processing, meaning this stream should be kept
   * alive until the application is finished communicating with the peer/socket.
   */
  def client: Stream[F, BlockchainPeerClient[F]] =
    Stream
      .eval(Deferred[F, Unit])
      .flatMap(deferred =>
        Stream(new BlockchainPeerClient[F] {

          override def remotePeer: ConnectedPeer = connectedPeer

          override def remotePeerAsServer: F[Option[KnownHost]] =
            writeRequest(BlockchainMultiplexerId.RemotePeerServerRequest, (), multiplexerBuffers.remotePeerServer)

          override def remotePeerAdoptions: F[Stream[F, BlockId]] =
            Async[F].delay(
              Stream
                .fromQueueUnterminated(cache.remoteBlockAdoptions)
                .interruptWhen(deferred.imap(_.asRight[Throwable])(_ => ()))
            )

          override def remoteTransactionNotifications: F[Stream[F, TransactionId]] =
            Async[F].delay(
              Stream
                .fromQueueUnterminated(cache.remoteTransactionAdoptions)
                .interruptWhen(deferred.imap(_.asRight[Throwable])(_ => ()))
            )

          override def getRemoteBlockIdAtDepth(depth: Long): F[Option[BlockId]] =
            writeRequest(BlockchainMultiplexerId.BlockIdAtDepthRequest, depth, multiplexerBuffers.blockIdAtDepth)

          override def getRemoteSlotData(id: BlockId): F[Option[SlotData]] =
            writeRequest(BlockchainMultiplexerId.SlotDataRequest, id, multiplexerBuffers.slotData)

          override def getRemoteHeader(id: BlockId): F[Option[BlockHeader]] =
            writeRequest(BlockchainMultiplexerId.HeaderRequest, id, multiplexerBuffers.headers)

          override def getRemoteBody(id: BlockId): F[Option[BlockBody]] =
            writeRequest(BlockchainMultiplexerId.BodyRequest, id, multiplexerBuffers.bodies)

          override def getRemoteTransaction(id: TransactionId): F[Option[IoTransaction]] =
            writeRequest(BlockchainMultiplexerId.TransactionRequest, id, multiplexerBuffers.transactions)

          override def getRemoteBlockIdAtHeight(height: Long): F[Option[BlockId]] =
            writeRequest(BlockchainMultiplexerId.BlockIdAtHeightRequest, height, multiplexerBuffers.blockIdAtHeight)

          override def getRemoteKnownHosts(request: CurrentKnownHostsReq): F[Option[CurrentKnownHostsRes]] =
            writeRequest(BlockchainMultiplexerId.KnownHostsRequest, request, multiplexerBuffers.knownHosts)

          override def getPongMessage(request: PingMessage): F[Option[PongMessage]] =
            writeRequest(BlockchainMultiplexerId.PingRequest, request, multiplexerBuffers.pingPong)

          override def notifyAboutThisNetworkLevel(networkLevel: Boolean): F[Unit] =
            writeRequest(BlockchainMultiplexerId.AppLevelRequest, networkLevel, multiplexerBuffers.appLevel)

          override def closeConnection(): F[Unit] = deferred.complete(()).void
        })
          .mergeHaltBoth(Stream.eval(deferred.get).drain)
          .mergeHaltBoth((background ++ Stream.eval(deferred.complete(()))).drain)
      )

  private def background: Stream[F, Unit] =
    readerStream.concurrently(portQueueStreams.merge(cacheStreams))

  /**
   * Parse the incoming multiplexer frames into either a request or response message.  Once parsed,
   * handle the payload accordingly.
   */
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
      multiplexerBuffers.knownHosts.backgroundRequestProcessor(_ => onPeerRequestedKnownHosts()),
      multiplexerBuffers.remotePeerServer.backgroundRequestProcessor(_ => onPeerRequestedRemotePeerServer()),
      multiplexerBuffers.blockAdoptions.backgroundRequestProcessor(_ => onPeerRequestedBlockNotification()),
      multiplexerBuffers.transactionAdoptions.backgroundRequestProcessor(_ => onPeerRequestedTransactionNotification()),
      multiplexerBuffers.pingPong.backgroundRequestProcessor(onPeerRequestedPing),
      multiplexerBuffers.blockIdAtHeight.backgroundRequestProcessor(onPeerRequestedBlockIdAtHeight),
      multiplexerBuffers.blockIdAtDepth.backgroundRequestProcessor(onPeerRequestedBlockIdAtDepth),
      multiplexerBuffers.slotData.backgroundRequestProcessor(onPeerRequestedSlotData),
      multiplexerBuffers.headers.backgroundRequestProcessor(onPeerRequestedHeader),
      multiplexerBuffers.bodies.backgroundRequestProcessor(onPeerRequestedBody),
      multiplexerBuffers.transactions.backgroundRequestProcessor(onPeerRequestedTransaction),
      multiplexerBuffers.appLevel.backgroundRequestProcessor(onPeerNotifiedAppLevel)
    ).parJoinUnbounded

  private def cacheStreams =
    Stream(
      Stream
        .force(server.localBlockAdoptions)
        .evalMap(
          cache.localBlockAdoptions
            .tryOffer(_)
            .flatMap(
              Async[F].raiseUnless(_)(new IllegalStateException("Slow peer subscriber of local block adoptions"))
            )
        )
        .void,
      Stream
        .force(server.localTransactionNotifications)
        .evalMap(
          cache.localTransactionAdoptions
            .tryOffer(_)
            .flatMap(
              Async[F]
                .raiseUnless(_)(new IllegalStateException("Slow peer subscriber of local transaction notifications"))
            )
        )
        .void,
      Stream
        .repeatEval(
          writeRequestNoTimeout(BlockchainMultiplexerId.BlockAdoptionRequest, (), multiplexerBuffers.blockAdoptions)
        )
        .evalMap(
          cache.remoteBlockAdoptions
            .tryOffer(_)
            .flatMap(
              Async[F].raiseUnless(_)(new IllegalStateException("Slow local subscriber of peer block adoptions"))
            )
        )
        .void,
      Stream
        .repeatEval(
          writeRequestNoTimeout(
            BlockchainMultiplexerId.TransactionNotificationRequest,
            (),
            multiplexerBuffers.transactionAdoptions
          )
        )
        .evalMap(
          cache.remoteTransactionAdoptions
            .tryOffer(_)
            .flatMap(
              Async[F]
                .raiseUnless(_)(new IllegalStateException("Slow local subscriber of peer transaction notifications"))
            )
        )
    ).parJoinUnbounded

  private def processRequest(port: Int, data: Bytes): F[Unit] =
    OptionT
      .fromOption[F](BlockchainMultiplexerId.parse(port))
      .getOrRaise(new IllegalArgumentException(s"Invalid port=$port"))
      .flatMap {
        case BlockchainMultiplexerId.KnownHostsRequest =>
          multiplexerBuffers.knownHosts.processRequest(tDecoded[CurrentKnownHostsReq](data))
        case BlockchainMultiplexerId.RemotePeerServerRequest =>
          multiplexerBuffers.remotePeerServer.processRequest(())
        case BlockchainMultiplexerId.BlockAdoptionRequest =>
          multiplexerBuffers.blockAdoptions.processRequest(())
        case BlockchainMultiplexerId.TransactionNotificationRequest =>
          multiplexerBuffers.transactionAdoptions.processRequest(())
        case BlockchainMultiplexerId.PingRequest =>
          multiplexerBuffers.pingPong.processRequest(tDecoded[PingMessage](data))
        case BlockchainMultiplexerId.BlockIdAtHeightRequest =>
          multiplexerBuffers.blockIdAtHeight.processRequest(tDecoded[Long](data))
        case BlockchainMultiplexerId.BlockIdAtDepthRequest =>
          multiplexerBuffers.blockIdAtDepth.processRequest(tDecoded[Long](data))
        case BlockchainMultiplexerId.SlotDataRequest =>
          multiplexerBuffers.slotData.processRequest(tDecoded[BlockId](data))
        case BlockchainMultiplexerId.HeaderRequest =>
          multiplexerBuffers.headers.processRequest(tDecoded[BlockId](data))
        case BlockchainMultiplexerId.BodyRequest =>
          multiplexerBuffers.bodies.processRequest(tDecoded[BlockId](data))
        case BlockchainMultiplexerId.TransactionRequest =>
          multiplexerBuffers.transactions.processRequest(tDecoded[TransactionId](data))
        case BlockchainMultiplexerId.AppLevelRequest =>
          multiplexerBuffers.appLevel.processRequest(tDecoded[Boolean](data))
      }

  private def processResponse(port: Int, data: Bytes): F[Unit] =
    OptionT
      .fromOption[F](BlockchainMultiplexerId.parse(port))
      .getOrRaise(new IllegalArgumentException(s"Invalid port=$port"))
      .flatMap {
        case BlockchainMultiplexerId.KnownHostsRequest =>
          multiplexerBuffers.knownHosts.processResponse(tDecoded[Option[CurrentKnownHostsRes]](data))
        case BlockchainMultiplexerId.RemotePeerServerRequest =>
          multiplexerBuffers.remotePeerServer.processResponse(tDecoded[Option[KnownHost]](data))
        case BlockchainMultiplexerId.BlockAdoptionRequest =>
          multiplexerBuffers.blockAdoptions.processResponse(tDecoded[BlockId](data))
        case BlockchainMultiplexerId.TransactionNotificationRequest =>
          multiplexerBuffers.transactionAdoptions.processResponse(tDecoded[TransactionId](data))
        case BlockchainMultiplexerId.PingRequest =>
          multiplexerBuffers.pingPong.processResponse(tDecoded[Option[PongMessage]](data))
        case BlockchainMultiplexerId.BlockIdAtHeightRequest =>
          multiplexerBuffers.blockIdAtHeight.processResponse(tDecoded[Option[BlockId]](data))
        case BlockchainMultiplexerId.BlockIdAtDepthRequest =>
          multiplexerBuffers.blockIdAtDepth.processResponse(tDecoded[Option[BlockId]](data))
        case BlockchainMultiplexerId.SlotDataRequest =>
          multiplexerBuffers.slotData.processResponse(tDecoded[Option[SlotData]](data))
        case BlockchainMultiplexerId.HeaderRequest =>
          multiplexerBuffers.headers.processResponse(tDecoded[Option[BlockHeader]](data))
        case BlockchainMultiplexerId.BodyRequest =>
          multiplexerBuffers.bodies.processResponse(tDecoded[Option[BlockBody]](data))
        case BlockchainMultiplexerId.TransactionRequest =>
          multiplexerBuffers.transactions.processResponse(tDecoded[Option[IoTransaction]](data))
        case BlockchainMultiplexerId.AppLevelRequest =>
          multiplexerBuffers.appLevel.processResponse(())
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
    buffer.awaitResponse

  private def writeResponse[Message: Transmittable](
    port:    BlockchainMultiplexerId,
    message: Message
  ): F[Unit] =
    readerWriter.write(port.id, OneBS.concat(Transmittable[Message].transmittableBytes(message)))

  private def onPeerRequestedBlockIdAtHeight(height: Long) =
    server
      .getLocalBlockAtHeight(height)
      .flatMap(writeResponse(BlockchainMultiplexerId.BlockIdAtHeightRequest, _))

  private def onPeerRequestedBlockIdAtDepth(depth: Long) =
    server
      .getLocalBlockAtDepth(depth)
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

/**
 * A buffer for inbound and outbound stream-based data, specific to a peer
 *
 * @param localBlockAdoptions a buffer containing BlockIds to be sent to the peer
 * @param localTransactionAdoptions a buffer containing TransactionIds to be sent to the peer
 * @param remoteBlockAdoptions a buffer containing BlockIds sent by the peer
 * @param remoteTransactionAdoptions a buffer containing TransactionIds sent by the peer
 */
class PeerStreamBuffer[F[_]](
  val localBlockAdoptions:        Queue[F, BlockId],
  val localTransactionAdoptions:  Queue[F, TransactionId],
  val remoteBlockAdoptions:       Queue[F, BlockId],
  val remoteTransactionAdoptions: Queue[F, TransactionId]
)

object PeerStreamBuffer {

  def make[F[_]: Async]: Resource[F, PeerStreamBuffer[F]] =
    (
      Queue.bounded[F, BlockId](256),
      Queue.bounded[F, TransactionId](256),
      Queue.bounded[F, BlockId](256),
      Queue.bounded[F, TransactionId](256)
    )
      .mapN(new PeerStreamBuffer[F](_, _, _, _))
      .toResource
}
