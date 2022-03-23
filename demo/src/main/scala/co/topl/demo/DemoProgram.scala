package co.topl.demo

import akka.NotUsed
import akka.actor.typed.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.{BroadcastHub, Keep, RunnableGraph, Sink, Source}
import cats.data.{EitherT, OptionT, Validated}
import cats.effect._
import cats.effect.std.Supervisor
import cats.implicits._
import cats.{~>, Applicative, Monad, MonadThrow, Show}
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
import scala.concurrent.duration.DurationInt

object DemoProgram {

  /**
   * A forever-running program which traverses epochs and the slots within the epochs
   */
  def run[F[_]: MonadThrow: Logger: Async: *[_] ~> Future](
    mint:               PerpetualBlockMintAlgebra[F],
    headerValidation:   BlockHeaderValidationAlgebra[F],
    headerStore:        Store[F, BlockHeaderV2],
    bodyStore:          Store[F, BlockBodyV2],
    localChain:         LocalChainAlgebra[F],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF],
    bindPort:           Int,
    remotePeers:        Source[InetSocketAddress, _]
  )(implicit system:    ActorSystem[_]): F[Unit] =
    for {
      (blockchainServer, onAdoptCallback) <- blockchainProtocolServer(headerStore, bodyStore)
      processBlock = (blockV2: BlockV2) =>
        processMintedBlock[F](
          blockV2,
          headerValidation,
          headerStore,
          bodyStore,
          localChain,
          ed25519VrfResource,
          onAdoptCallback
        )
      p2pServer         <- networking(bindPort, remotePeers, headerStore, bodyStore, blockchainServer, processBlock)
      mintedBlockStream <- mint.blocks
      streamCompletionFuture = implicitly[RunnableGraph ~> F].apply(
        mintedBlockStream
          .mapAsyncF(1)(block =>
            Logger[F].info(show"Minted block ${block.headerV2}") >>
            processBlock(block)
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
    headerStore:        Store[F, BlockHeaderV2],
    bodyStore:          Store[F, BlockBodyV2],
    localChain:         LocalChainAlgebra[F],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF],
    onAdopt:            TypedIdentifier => F[Unit]
  ): F[Unit] =
    for {
      _                     <- headerStore.put(nextBlock.headerV2.id, nextBlock.headerV2)
      _                     <- bodyStore.put(nextBlock.headerV2.id, nextBlock.blockBodyV2)
      slotData              <- ed25519VrfResource.use(implicit ed25519Vrf => nextBlock.headerV2.slotData.pure[F])
      localChainIsWorseThan <- localChain.isWorseThan(slotData)
      _ <- Monad[F].ifElseM(
        localChainIsWorseThan.pure[F] ->
        Sync[F].defer(
          EitherT(
            OptionT(headerStore.get(nextBlock.headerV2.parentHeaderId))
              .getOrElseF(MonadThrow[F].raiseError(new NoSuchElementException(nextBlock.headerV2.parentHeaderId.show)))
              .flatMap(parent => headerValidation.validate(nextBlock.headerV2, parent))
          )
            // TODO: Now fetch the body from the network and validate against the ledger
            .semiflatTap(_ => localChain.adopt(Validated.Valid(slotData)))
            .semiflatTap(header => onAdopt(header.id))
            .semiflatTap(header =>
              Logger[F].info(
                show"Adopted head block id=${header.id.asTypedBytes} height=${header.height} slot=${header.slot}"
              )
            )
            .void
            .valueOrF(e =>
              Logger[F]
                .warn(show"Invalid block header. reason=$e block=${nextBlock.headerV2}")
                // TODO: Penalize the peer
                .flatTap(_ =>
                  headerStore.remove(nextBlock.headerV2.id).tupleRight(bodyStore.remove(nextBlock.headerV2.id))
                )
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
    bindPort:        Int,
    remotePeers:     Source[InetSocketAddress, _],
    headerStore:     Store[F, BlockHeaderV2],
    bodyStore:       Store[F, BlockBodyV2],
    server:          BlockchainProtocolServer[F],
    onBlockReceived: BlockV2 => F[Unit]
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
            .flatTap(peerConnection =>
              handleNetworkClient(peerConnection.client, headerStore, bodyStore, onBlockReceived)
            )
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

  private def blockchainProtocolServer[F[_]: Sync](
    headerStore: Store[F, BlockHeaderV2],
    bodyStore:   Store[F, BlockBodyV2]
  )(implicit
    materializer: Materializer
  ): F[(BlockchainProtocolServer[F], TypedIdentifier => F[Unit])] =
    for {
      (locallyMintedBlockIdsQueue, locallyMintedBlockIdsSource) <-
        Sync[F].delay(
          Source
            .queue[TypedIdentifier](128)
            .toMat(BroadcastHub.sink)(Keep.both)
            .run()
        )
      server = new BlockchainProtocolServer[F] {

        def localBlockAdoptions: F[Source[TypedIdentifier, NotUsed]] =
          Sync[F].delay(locallyMintedBlockIdsSource)

        def getLocalHeader(id: TypedIdentifier): F[Option[BlockHeaderV2]] = headerStore.get(id)

        def getLocalBody(id: TypedIdentifier): F[Option[BlockBodyV2]] = bodyStore.get(id)
      }
    } yield (server, (id: TypedIdentifier) => Sync[F].delay(locallyMintedBlockIdsQueue.offer(id)))

  private def handleNetworkClient[F[_]: Async: Concurrent: Logger: *[_] ~> Future](
    client:          BlockchainProtocolClient[F],
    headerStore:     Store[F, BlockHeaderV2],
    bodyStore:       Store[F, BlockBodyV2],
    onBlockReceived: BlockV2 => F[Unit]
  )(implicit system: ActorSystem[_]): F[Unit] =
    for {
      _ <- Supervisor[F].use(
        _.supervise(
          for {
            remoteAdoptionsSource <- client.remotePeerAdoptions
            processor = remoteAdoptionsSource.mapAsyncF(1)(id =>
              headerStore
                .get(id)
                .flatMap(maybeCurrentHeader =>
                  (id, maybeCurrentHeader.isEmpty)
                    .iterateWhileM { case (id, _) =>
                      OptionT(headerStore.get(id))
                        .flatTapNone(
                          OptionT
                            .pure[F](Logger[F].info(show"Requesting header id=$id"))
                            .flatMap(_ =>
                              OptionT(client.getRemoteHeader(id))
                                .semiflatTap(header => Logger[F].info(show"Inserting remote header id=$id"))
                                .flatTapNone(Logger[F].info(show"Remote did not possess header id=$id"))
                                .semiflatTap(header => headerStore.put(header.id, header))
                            )
                            .value
                        )
                        .value
                        .flatMap(_ =>
                          OptionT(bodyStore.get(id))
                            .flatTapNone(
                              OptionT
                                .pure[F](Logger[F].info(show"Requesting body id=$id"))
                                .flatMap(_ => OptionT(client.getRemoteBody(id)))
                                .semiflatTap(body => Logger[F].info(show"Inserting remote body id=$id"))
                                .flatTapNone(Logger[F].info(show"Remote did not possess body id=$id"))
                                .semiflatTap(body => bodyStore.put(id, body))
                                .value
                            )
                            .value
                        )
                        .flatMap(_ =>
                          OptionT(headerStore.get(id))
                            .map(_.parentHeaderId)
                            .getOrElse(???)
                            .flatMap(parentId => OptionT(headerStore.get(id)).isEmpty.tupleLeft(parentId))
                        )
                    }(_._2)
                    .flatMap(_ =>
                      Applicative[F].whenA(maybeCurrentHeader.isEmpty)(
                        Sync[F].defer(
                          (OptionT(headerStore.get(id)), OptionT(bodyStore.get(id)))
                            .mapN((header, body) => BlockV2(header, body))
                            .semiflatTap(_ => Logger[F].info(show"Processing remote block id=$id"))
                            .semiflatTap(onBlockReceived)
                            .value
                        )
                      )
                    )
                )
            )
            completion <- Async[F].fromFuture(Async[F].delay(processor.runWith(Sink.ignore)))
          } yield ()
        )
      )
    } yield ()

}
