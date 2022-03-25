package co.topl.networking.blockchain

import akka.actor.typed.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import akka.{Done, NotUsed}
import cats.data.OptionT
import cats.effect.std.Supervisor
import cats.effect.{Async, Concurrent, Fiber, Sync}
import cats.implicits._
import cats.{~>, Applicative}
import co.topl.algebras.Store
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.models.{BlockBodyV2, BlockHeaderV2, BlockV2, TypedIdentifier}
import co.topl.networking.p2p._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import java.net.InetSocketAddress
import scala.concurrent.Future

object BlockchainNetwork {

  def make[F[_]: Async: Logger: *[_] ~> Future](
    bindPort:               Int,
    remotePeers:            Source[InetSocketAddress, _],
    headerStore:            Store[F, BlockHeaderV2],
    bodyStore:              Store[F, BlockBodyV2],
    locallyAdoptedBlockIds: Source[TypedIdentifier, NotUsed],
    onBlockReceived:        BlockV2 => F[Unit]
  )(implicit
    system: ActorSystem[_]
  ): F[P2PServer[F, BlockchainPeerClient[F]]] =
    for {
      localAddress <- InetSocketAddress.createUnresolved("localhost", bindPort).pure[F]
      localPeer = LocalPeer(localAddress)
      blockchainServer <- blockchainProtocolServer(headerStore, bodyStore, locallyAdoptedBlockIds)
      connectionFlowFactory = BlockchainPeerConnectionFlowFactory.make[F](blockchainServer)
      peerHandlerFlow =
        (connectedPeer: ConnectedPeer) =>
          ConnectionLeaderFlow(leader =>
            Flow.futureFlow(
              implicitly[F ~> Future].apply(connectionFlowFactory(connectedPeer, leader))
            )
          )
            .mapMaterializedValue(f => Async[F].fromFuture(f.flatten.pure[F]))
            .pure[F]
      p2pServer <- {
        implicit val classicSystem = system.classicSystem
        AkkaP2PServer.make[F, BlockchainPeerClient[F]](
          "localhost",
          bindPort,
          localAddress,
          remotePeers = remotePeers,
          peerHandlerFlow
        )
      }
      _ <- p2pServer.newConnectedPeers.flatMap(peers =>
        handleNetworkClients(peers.map(_._2), headerStore, bodyStore, onBlockReceived)
      )
      _ <- Logger[F].info(s"Bound P2P at host=localhost port=$bindPort")
    } yield p2pServer

  private def blockchainProtocolServer[F[_]: Sync](
    headerStore:            Store[F, BlockHeaderV2],
    bodyStore:              Store[F, BlockBodyV2],
    locallyAdoptedBlockIds: Source[TypedIdentifier, NotUsed]
  )(implicit
    materializer: Materializer
  ): F[BlockchainPeerServer[F]] =
    Sync[F].delay(
      new BlockchainPeerServer[F] {

        def localBlockAdoptions: F[Source[TypedIdentifier, NotUsed]] =
          locallyAdoptedBlockIds.pure[F]

        def getLocalHeader(id: TypedIdentifier): F[Option[BlockHeaderV2]] = headerStore.get(id)

        def getLocalBody(id: TypedIdentifier): F[Option[BlockBodyV2]] = bodyStore.get(id)
      }
    )

  private def handleNetworkClients[F[_]: Async: Concurrent: Logger: *[_] ~> Future](
    clients:         Source[BlockchainPeerClient[F], _],
    headerStore:     Store[F, BlockHeaderV2],
    bodyStore:       Store[F, BlockBodyV2],
    onBlockReceived: BlockV2 => F[Unit]
  )(implicit system: ActorSystem[_]): F[Fiber[F, Throwable, Done]] =
    Supervisor[F].use(
      _.supervise(
        Async[F].fromFuture(
          clients
            .mapAsyncF(1)(handleNetworkClient(_, headerStore, bodyStore, onBlockReceived))
            .toMat(Sink.ignore)(Keep.right)
            .mapK[F]
        )
      )
    )

  private def handleNetworkClient[F[_]: Async: Concurrent: Logger: *[_] ~> Future](
    client:          BlockchainPeerClient[F],
    headerStore:     Store[F, BlockHeaderV2],
    bodyStore:       Store[F, BlockBodyV2],
    onBlockReceived: BlockV2 => F[Unit]
  )(implicit system: ActorSystem[_]): F[Fiber[F, Throwable, Unit]] =
    for {
      remoteAdoptionsSource <- client.remotePeerAdoptions
      blockNotificationProcessor = processRemoteBlockNotification[F](
        client,
        headerStore,
        bodyStore,
        onBlockReceived
      ) _
      s <- Supervisor[F].use(
        _.supervise(
          Async[F]
            .fromFuture(
              remoteAdoptionsSource
                .mapAsyncF(1)(blockNotificationProcessor)
                .toMat(Sink.ignore)(Keep.right)
                .mapK[F]
            )
            .void
        )
      )
    } yield s

  private def processRemoteBlockNotification[F[_]: Async: Concurrent: Logger: *[_] ~> Future](
    client:          BlockchainPeerClient[F],
    headerStore:     Store[F, BlockHeaderV2],
    bodyStore:       Store[F, BlockBodyV2],
    onBlockReceived: BlockV2 => F[Unit]
  )(id:              TypedIdentifier) =
    Logger[F].info(show"Remote peer adopted block id=$id") >>
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
                  .flatMap(parentId => OptionT(headerStore.get(parentId)).isEmpty.tupleLeft(parentId))
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

}
