package co.topl.demo

import akka.NotUsed
import akka.actor.typed.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.{BroadcastHub, Keep, RunnableGraph, Sink, Source}
import cats.data.{EitherT, OptionT, Validated}
import cats.effect._
import cats.effect.std.Supervisor
import cats.implicits._
import cats.{~>, Monad, MonadThrow, Show}
import co.topl.algebras.{Store, UnsafeResource}
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras.{BlockHeaderValidationAlgebra, LocalChainAlgebra}
import co.topl.consensus.{BlockHeaderV2Ops, BlockHeaderValidationFailure}
import co.topl.crypto.signing.Ed25519VRF
import co.topl.minting.algebras.PerpetualBlockMintAlgebra
import co.topl.models._
import co.topl.networking.blockchain.{BlockchainPeerConnection, BlockchainProtocolClient, BlockchainProtocolServer}
import co.topl.networking.p2p._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import java.net.InetSocketAddress
import scala.concurrent.Future

object DemoProgram {

  /**
   * A forever-running program which traverses epochs and the slots within the epochs
   */
  def run[F[_]: MonadThrow: Logger: Async: *[_] ~> Future](
    mint:               PerpetualBlockMintAlgebra[F],
    headerValidation:   BlockHeaderValidationAlgebra[F],
    headerStore:        Store[F, BlockHeaderV2],
    blockStore:         Store[F, BlockV2],
    localChain:         LocalChainAlgebra[F],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF],
    bindPort:           Int,
    remotePeers:        Source[InetSocketAddress, _]
  )(implicit system:    ActorSystem[_]): F[Unit] =
    for {
      (blockchainServer, onAdoptCallback) <- blockchainProtocolServer(headerStore)
      p2pServer                           <- networking(bindPort, remotePeers, headerStore, blockchainServer)
//      (p2pServer, onAdoptCallback) <- networking(bindPort, remotePeers, headerStore)
//      onAdoptCallback   <- ((_: TypedIdentifier) => Applicative[F].unit).pure[F]
      mintedBlockStream <- mint.blocks
      streamCompletionFuture = implicitly[RunnableGraph ~> F].apply(
        mintedBlockStream
          .mapAsyncF(1)(
            processMintedBlock[F](_, headerValidation, blockStore, localChain, ed25519VrfResource, onAdoptCallback)
          )
          .toMat(Sink.ignore)(Keep.right)
      )
      _ <- Async[F]
        .fromFuture(streamCompletionFuture)
        .void
    } yield ()

  implicit private val showBlockHeaderValidationFailure: Show[BlockHeaderValidationFailure] =
    Show.fromToString

  /**
   * Insert block to local storage and perform chain selection.  If better, validate the block and then adopt it locally.
   */
  private def processMintedBlock[F[_]: MonadThrow: Sync: Logger](
    nextBlock:          BlockV2,
    headerValidation:   BlockHeaderValidationAlgebra[F],
    blockStore:         Store[F, BlockV2],
    localChain:         LocalChainAlgebra[F],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF],
    onAdopt:            TypedIdentifier => F[Unit]
  ): F[Unit] =
    for {
      _                     <- Logger[F].info(show"Minted block ${nextBlock.headerV2}")
      _                     <- blockStore.put(nextBlock.headerV2.id, nextBlock)
      slotData              <- ed25519VrfResource.use(implicit ed25519Vrf => nextBlock.headerV2.slotData.pure[F])
      localChainIsWorseThan <- localChain.isWorseThan(slotData)
      _ <- Monad[F].ifElseM(
        localChainIsWorseThan.pure[F] ->
        Sync[F].defer(
          EitherT(
            OptionT(blockStore.get(nextBlock.headerV2.parentHeaderId))
              .getOrElseF(MonadThrow[F].raiseError(new NoSuchElementException(nextBlock.headerV2.parentHeaderId.show)))
              .flatMap(parent => headerValidation.validate(nextBlock.headerV2, parent.headerV2))
          )
            // TODO: Now fetch the body from the network and validate against the ledger
            .semiflatTap(_ => localChain.adopt(Validated.Valid(slotData)))
            .semiflatTap(header => onAdopt(header.id))
            .semiflatTap(header => Logger[F].info(show"Adopted local head block id=${header.id}"))
            .void
            .valueOrF(e =>
              Logger[F]
                .warn(show"Invalid block header. reason=$e block=${nextBlock.headerV2}")
                // TODO: Penalize the peer
                .flatTap(_ => blockStore.remove(nextBlock.headerV2.id))
            )
        )
      )(
        Logger[F].info(show"Ignoring weaker block header id=${nextBlock.headerV2.id}")
      )
    } yield ()

  /**
   * @return (P2PServer, Callback to invoke when locally adopting a block)
   */
  private def networking[F[_]: Async: Logger: *[_] ~> Future](
    bindPort:    Int,
    remotePeers: Source[InetSocketAddress, _],
    headerStore: Store[F, BlockHeaderV2],
    server:      BlockchainProtocolServer[F]
  )(implicit
    system: ActorSystem[_]
  ): F[P2PServer[F]] =
    for {
      localAddress <- InetSocketAddress.createUnresolved("localhost", bindPort).pure[F]
      localPeer = LocalPeer(localAddress)
      peerHandlerFlow =
        (connectedPeer: ConnectedPeer, leader: ConnectionLeader) =>
          BlockchainPeerConnection
            .make[F](connectedPeer, leader, localPeer)(server)
            .flatTap(peerConnection => handleNetworkClient(peerConnection.client, headerStore))
            .map(peerConnection => peerConnection.multiplexer)
      p2pServer <- {
        implicit val classicSystem = system.classicSystem
        AkkaP2PServer.make(
          "localhost",
          bindPort,
          localAddress,
          remotePeers = remotePeers,
          peerHandlerFlow
        )
      }
      _ <- Logger[F].info(s"Bound P2P at host=localhost port=$bindPort")
    } yield p2pServer

  private def blockchainProtocolServer[F[_]: Sync](headerStore: Store[F, BlockHeaderV2])(implicit
    materializer:                                               Materializer
  ): F[(BlockchainProtocolServer[F], TypedIdentifier => F[Unit])] =
    for {
      (locallyMintedBlockIdsQueue, locallyMintedBlockIdsSource) <-
        Sync[F].delay(Source.queue[TypedIdentifier](128).toMat(BroadcastHub.sink)(Keep.both).run())
      server = new BlockchainProtocolServer[F] {
        def localBlockAdoptions: F[Source[TypedIdentifier, NotUsed]] = Sync[F].delay(locallyMintedBlockIdsSource)

        def getLocalHeader(id: TypedIdentifier): F[Option[BlockHeaderV2]] = headerStore.get(id)
      }
    } yield (server, (id: TypedIdentifier) => Sync[F].delay(locallyMintedBlockIdsQueue.offer(id)))

  private def handleNetworkClient[F[_]: Async: Concurrent: Logger: *[_] ~> Future](
    client:          BlockchainProtocolClient[F],
    headerStore:     Store[F, BlockHeaderV2]
  )(implicit system: ActorSystem[_]): F[Unit] =
    for {
      _ <- Supervisor[F].use(
        _.supervise(
          for {
            remoteAdoptionsSource <- client.remotePeerAdoptions
            processor = remoteAdoptionsSource.mapAsyncF(1)(id =>
              Logger[F].info(show"Requesting header id=$id") >>
              OptionT(client.getRemoteHeader(id))
                .semiflatTap(header => Logger[F].info(show"Inserting remote header id=$id"))
                .flatTapNone(Logger[F].info(show"Remote did not possess header id=$id"))
                .semiflatTap(header => headerStore.put(header.id, header))
                .value
            )
            completion <- Async[F].fromFuture(Async[F].delay(processor.runWith(Sink.ignore)))
          } yield ()
        )
      )
    } yield ()

}
