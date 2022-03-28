package co.topl.networking.blockchain

import akka.actor.typed.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import akka.{Done, NotUsed}
import cats.data.OptionT
import cats.effect.std.Supervisor
import cats.effect.{Async, Concurrent, Fiber, Sync}
import cats.implicits._
import cats.{~>, MonadThrow}
import co.topl.algebras.Store
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.models._
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
    transactionStore:       Store[F, Transaction],
    locallyAdoptedBlockIds: Source[TypedIdentifier, NotUsed],
    onBlockReceived:        BlockV2 => F[Unit]
  )(implicit
    system: ActorSystem[_]
  ): F[P2PServer[F, BlockchainPeerClient[F]]] =
    for {
      localAddress <- InetSocketAddress.createUnresolved("localhost", bindPort).pure[F]
      localPeer = LocalPeer(localAddress)
      blockchainServer <- blockchainProtocolServer(headerStore, bodyStore, transactionStore, locallyAdoptedBlockIds)
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
      peerChanges <- p2pServer.peerChanges
      peerClients = peerChanges.collect { case PeerConnectionChanges.ConnectionEstablished(_, client) => client }
      _ <- handleNetworkClients(peerClients, headerStore, bodyStore, transactionStore, onBlockReceived)
      _ <- Logger[F].info(s"Bound P2P at host=localhost port=$bindPort")
    } yield p2pServer

  private def blockchainProtocolServer[F[_]: Sync](
    headerStore:            Store[F, BlockHeaderV2],
    bodyStore:              Store[F, BlockBodyV2],
    transactionStore:       Store[F, Transaction],
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

        def getLocalTransaction(id: TypedIdentifier): F[Option[Transaction]] = transactionStore.get(id)
      }
    )

  private def handleNetworkClients[F[_]: Async: Concurrent: Logger: *[_] ~> Future](
    clients:          Source[BlockchainPeerClient[F], _],
    headerStore:      Store[F, BlockHeaderV2],
    bodyStore:        Store[F, BlockBodyV2],
    transactionStore: Store[F, Transaction],
    onBlockReceived:  BlockV2 => F[Unit]
  )(implicit system:  ActorSystem[_]): F[Fiber[F, Throwable, Done]] =
    Supervisor[F].use(
      _.supervise(
        Async[F].fromFuture(
          clients
            .mapAsyncF(1)(handleNetworkClient(_, headerStore, bodyStore, transactionStore, onBlockReceived))
            .toMat(Sink.ignore)(Keep.right)
            .liftTo[F]
        )
      )
    )

  private def handleNetworkClient[F[_]: Async: Concurrent: Logger: *[_] ~> Future](
    client:           BlockchainPeerClient[F],
    headerStore:      Store[F, BlockHeaderV2],
    bodyStore:        Store[F, BlockBodyV2],
    transactionStore: Store[F, Transaction],
    onBlockReceived:  BlockV2 => F[Unit]
  )(implicit system:  ActorSystem[_]): F[Fiber[F, Throwable, Unit]] =
    for {
      remoteAdoptionsSource <- client.remotePeerAdoptions
      blockNotificationProcessor = processRemoteBlockNotification[F](
        client,
        headerStore,
        bodyStore,
        transactionStore,
        onBlockReceived
      ) _
      s <- Supervisor[F].use(
        _.supervise(
          Async[F]
            .fromFuture(
              remoteAdoptionsSource
                .mapAsyncF(1)(blockNotificationProcessor)
                .toMat(Sink.ignore)(Keep.right)
                .liftTo[F]
            )
            .void
        )
      )
    } yield s

  private def processRemoteBlockNotification[F[_]: Async: Concurrent: Logger: *[_] ~> Future](
    client:           BlockchainPeerClient[F],
    headerStore:      Store[F, BlockHeaderV2],
    bodyStore:        Store[F, BlockBodyV2],
    transactionStore: Store[F, Transaction],
    onBlockReceived:  BlockV2 => F[Unit]
  )(id:               TypedIdentifier) =
    for {
      _                  <- Logger[F].info(show"Remote peer adopted block id=$id")
      maybeCurrentHeader <- headerStore.get(id)
      _ <- (id, maybeCurrentHeader)
        // Recursively fetch the remote header+body+transactions until a common ancestor is found
        .iterateWhileM[F] { case (id, _) =>
          fetchHeader(client, headerStore)(id)
            .productL(fetchBody(client, bodyStore)(id).flatMap(fetchTransactions(client, transactionStore)).void)
            .flatMap(header => headerStore.get(header.parentHeaderId).tupleLeft(header.parentHeaderId))
        }(_._2.isEmpty)
      _ <- OptionT
        .fromOption[F](maybeCurrentHeader)
        .flatTapNone(
          (OptionT(headerStore.get(id)), OptionT(bodyStore.get(id)))
            .mapN((header, body) => BlockV2(header, body))
            .semiflatTap(_ => Logger[F].info(show"Processing remote block id=$id"))
            .semiflatTap(onBlockReceived)
            .value
        )
        .value
    } yield ()

  private def fetchHeader[F[_]: Async: Concurrent: Logger: *[_] ~> Future](
    client:      BlockchainPeerClient[F],
    headerStore: Store[F, BlockHeaderV2]
  )(id:          TypedIdentifier) =
    OptionT(headerStore.get(id))
      .orElse(
        OptionT
          .pure[F](Logger[F].info(show"Requesting header id=$id"))
          .flatMap(_ =>
            OptionT(client.getRemoteHeader(id))
              // TODO:  Verify the locally computed header ID against `id`
              .semiflatTap(header => Logger[F].info(show"Inserting remote header id=$id"))
              .flatTapNone(Logger[F].info(show"Remote did not possess header id=$id"))
              .semiflatTap(header => headerStore.put(header.id, header))
          )
      )
      .getOrNoSuchElement(id.show)

  private def fetchBody[F[_]: Async: Concurrent: Logger: *[_] ~> Future](
    client:    BlockchainPeerClient[F],
    bodyStore: Store[F, BlockBodyV2]
  )(id:        TypedIdentifier) =
    OptionT(bodyStore.get(id))
      .orElse(
        OptionT
          .pure[F](Logger[F].info(show"Requesting body id=$id"))
          .flatMap(_ =>
            OptionT(client.getRemoteBody(id))
              // TODO: Verify the transaction IDs associated with this body match the txRoot of the header
              .semiflatTap(_ => Logger[F].info(show"Inserting remote body id=$id"))
              .flatTapNone(Logger[F].info(show"Remote did not possess body id=$id"))
              .semiflatTap(bodyStore.put(id, _))
          )
      )
      .getOrNoSuchElement(id.show)

  private def fetchTransactions[F[_]: Async: Concurrent: Logger: *[_] ~> Future](
    client:           BlockchainPeerClient[F],
    transactionStore: Store[F, Transaction]
  ) =
    (body: BlockBodyV2) => body.traverse(fetchTransaction[F](client, transactionStore))

  private def fetchTransaction[F[_]: Async: Concurrent: Logger: *[_] ~> Future](
    client:           BlockchainPeerClient[F],
    transactionStore: Store[F, Transaction]
  ) = { (id: TypedIdentifier) =>
    OptionT(transactionStore.get(id))
      .orElse(
        OptionT
          .pure[F](Logger[F].info(show"Requesting transaction id=$id")) >>
        OptionT(client.getRemoteTransaction(id))
          // TODO:  Verify the locally computed transaction ID against `id`
          .semiflatTap(_ => Logger[F].info(show"Inserting remote transaction id=$id"))
          .flatTapNone(Logger[F].info(show"Remote did not possess transaction id=$id"))
          .semiflatTap(transactionStore.put(id, _))
      )
      .getOrNoSuchElement(id.show)
  }

  implicit class OptionTOps[F[_], T](optionT: OptionT[F, T]) {

    def getOrNoSuchElement(id: Any)(implicit M: MonadThrow[F]): F[T] =
      optionT.getOrElseF(M.raiseError(new NoSuchElementException(id.toString)))
  }

}
