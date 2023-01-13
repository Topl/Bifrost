package co.topl.networking.blockchain

import akka.actor.typed.ActorSystem
import akka.stream.scaladsl.{BroadcastHub, Flow, Keep, Sink, Source}
import akka.util.ByteString
import cats.effect._
import cats.implicits._
import cats.{Applicative, MonadThrow, Parallel}
import co.topl.catsakka._
import co.topl.networking.p2p._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import scala.util.Random

object BlockchainNetwork {

  def make[F[_]: Parallel: Async: Logger: FToFuture](
    host:          String,
    bindPort:      Int,
    localPeer:     LocalPeer,
    remotePeers:   Source[DisconnectedPeer, _],
    clientHandler: BlockchainPeerHandlerAlgebra[F],
    server:        BlockchainPeerServer[F],
    peerFlowModifier: (
      ConnectedPeer,
      Flow[ByteString, ByteString, F[BlockchainPeerClient[F]]]
    ) => Flow[ByteString, ByteString, F[BlockchainPeerClient[F]]]
  )(implicit
    system: ActorSystem[_],
    random: Random
  ): F[(P2PServer[F, BlockchainPeerClient[F]], Fiber[F, Throwable, Unit])] =
    for {
      connectionFlowFactory <- BlockchainPeerConnectionFlowFactory.make[F](server).pure[F]
      peerHandlerFlow =
        (connectedPeer: ConnectedPeer) =>
          peerFlowModifier(
            connectedPeer,
            ConnectionLeaderFlow(leader =>
              Flow
                .fromMaterializer((_, _) =>
                  Flow.futureFlow(
                    implicitly[FToFuture[F]].apply(
                      connectionFlowFactory(connectedPeer, leader).allocated
                        .map { case (flow, finalizers) =>
                          flow.alsoTo(Sink.onComplete(_ => implicitly[FToFuture[F]].apply(finalizers)))
                        }
                    )
                  )
                )
                .mapMaterializedValue(_.flatten)
            )
              .mapMaterializedValue(f => Async[F].fromFuture(f.flatten.pure[F]))
          )
            .pure[F]
      p2pServer <- {
        implicit val classicSystem = system.classicSystem
        AkkaP2PServer.make[F, BlockchainPeerClient[F]](
          host,
          bindPort,
          localPeer,
          remotePeers = remotePeers,
          peerHandlerFlow
        )
      }
      peerChanges         <- p2pServer.peerChanges
      reusablePeerChanges <- peerChanges.toMat(BroadcastHub.sink)(Keep.right).liftTo[F]
      peerTerminations = reusablePeerChanges.collect { case c: PeerConnectionChanges.ConnectionClosed => c }
      _ <- Spawn[F].start(
        peerTerminations
          .mapAsyncF(1) {
            case PeerConnectionChanges.ConnectionClosed(_, Some(error)) =>
              Logger[F].error(error)("Peer connection terminated with error")
            case PeerConnectionChanges.ConnectionClosed(_, _) =>
              Logger[F].info("Peer connection terminated normally")
          }
          .toMat(Sink.ignore)(Keep.right)
          .liftTo[F]
      )
      peerClients = reusablePeerChanges.collect { case PeerConnectionChanges.ConnectionEstablished(_, client) =>
        client
      }
      clientFiber <- handleNetworkClients(peerClients, clientHandler)
      _           <- Logger[F].info(s"Bound P2P at host=$host port=$bindPort")
    } yield (p2pServer, clientFiber)

  private def handleNetworkClients[F[_]: Parallel: Async: Logger: FToFuture](
    clients:         Source[BlockchainPeerClient[F], _],
    clientHandler:   BlockchainPeerHandlerAlgebra[F]
  )(implicit system: ActorSystem[_]): F[Fiber[F, Throwable, Unit]] =
    Spawn[F].start(
      Async[F]
        .fromFuture(
          clients
            .mapAsyncF(1)(client =>
              Spawn[F].start(
                clientHandler
                  .usePeer(client)
                  .handleErrorWith(t =>
                    client.remotePeer
                      .flatMap(peer =>
                        Logger[F].error(t)(show"Client connection to remote=${peer.remoteAddress} failed")
                      )
                      .void
                  )
              )
            )
            .toMat(Sink.seq)(Keep.right)
            .liftTo[F]
        )
        .flatMap(t => t.parTraverse(_.join))
        .flatMap(_.foldMapM {
          case Outcome.Succeeded(_) => Applicative[F].unit
          case Outcome.Canceled()   => Applicative[F].unit
          case Outcome.Errored(e)   => MonadThrow[F].raiseError(e).void
        })
    )

}
