package co.topl.networking.blockchain

import akka.actor.typed.ActorSystem
import akka.stream.scaladsl.{Flow, Sink}
import akka.util.ByteString
import cats.effect._
import cats.implicits._
import co.topl.catsakka._
import co.topl.networking.p2p._
import co.topl.typeclasses.implicits._
import fs2._
import fs2.concurrent.Topic
import org.typelevel.log4cats.Logger

import scala.util.Random

object BlockchainNetwork {

  def make[F[_]: Async: Logger: FToFuture](
    host:          String,
    bindPort:      Int,
    localPeer:     LocalPeer,
    remotePeers:   Stream[F, DisconnectedPeer],
    clientHandler: BlockchainPeerHandlerAlgebra[F],
    server:        BlockchainPeerServer[F],
    peerFlowModifier: (
      ConnectedPeer,
      Flow[ByteString, ByteString, F[BlockchainPeerClient[F]]]
    ) => Flow[ByteString, ByteString, F[BlockchainPeerClient[F]]]
  )(implicit
    system: ActorSystem[_],
    random: Random
  ): Resource[F, P2PServer[F, BlockchainPeerClient[F]]] =
    for {
      connectionFlowFactory <- Resource.eval(BlockchainPeerConnectionFlowFactory.make[F](server).pure[F])
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
      peerChangesTopic <- Resource.make(Topic[F, PeerConnectionChange[BlockchainPeerClient[F]]])(_.close.void)
      _ <- Async[F].background(
        p2pServer.peerChanges.map(_.subscribeUnbounded.through(peerChangesTopic.publish).compile.drain)
      )
      _ <- peerChangesTopic.subscribeAwaitUnbounded
        .flatMap(changes =>
          Async[F].background(
            changes
              .collect { case c: PeerConnectionChanges.ConnectionClosed => c }
              .evalMap {
                case PeerConnectionChanges.ConnectionClosed(_, Some(error)) =>
                  Logger[F].error(error)("Peer connection terminated with error")
                case PeerConnectionChanges.ConnectionClosed(_, _) =>
                  Logger[F].info("Peer connection terminated normally")
              }
              .compile
              .drain
          )
        )
      peerClients <-
        peerChangesTopic.subscribeAwaitUnbounded.map(
          _.collect { case PeerConnectionChanges.ConnectionEstablished(_, client) => client }
        )
      _ <- handleNetworkClients(peerClients, clientHandler)
      _ <- Resource.eval(Logger[F].info(s"Bound P2P at host=$host port=$bindPort"))
    } yield p2pServer

  private def handleNetworkClients[F[_]: Async: Logger: FToFuture](
    clients:       Stream[F, BlockchainPeerClient[F]],
    clientHandler: BlockchainPeerHandlerAlgebra[F]
  ) =
    Async[F]
      .background(
        clients
          .parEvalMapUnbounded(client =>
            clientHandler
              .usePeer(client)
              .handleErrorWith(t =>
                client.remotePeer
                  .flatMap(peer => Logger[F].error(t)(show"Client connection to remote=${peer.remoteAddress} failed"))
                  .void
              )
          )
          .compile
          .drain
      )

}
