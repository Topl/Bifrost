package co.topl.networking.blockchain

import akka.actor.typed.ActorSystem
import akka.stream.scaladsl.{Flow, Sink}
import cats.Parallel
import cats.effect._
import cats.implicits._
import co.topl.catsakka._
import co.topl.networking.p2p._
import fs2._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.util.Random

object BlockchainNetwork {

  def make[F[_]: Async: Parallel: FToFuture](
    host:          String,
    bindPort:      Int,
    localPeer:     LocalPeer,
    remotePeers:   Stream[F, DisconnectedPeer],
    clientHandler: BlockchainPeerHandlerAlgebra[F],
    serverF:       ConnectedPeer => Resource[F, BlockchainPeerServerAlgebra[F]]
  )(implicit
    system: ActorSystem[_],
    random: Random
  ): Resource[F, P2PServer[F, BlockchainPeerClient[F]]] =
    for {
      implicit0(logger: Logger[F]) <- Resource.eval(Slf4jLogger.getLoggerFromName("Bifrost.P2P").pure[F])
      connectionFlowFactory = BlockchainPeerConnectionFlowFactory.make[F](serverF)
      peerHandlerFlow =
        (connectedPeer: ConnectedPeer) =>
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
            .pure[F]
      p2pServer <- {
        implicit val classicSystem: akka.actor.ActorSystem = system.classicSystem
        AkkaP2PServer.make[F, BlockchainPeerClient[F]](
          host,
          bindPort,
          localPeer,
          remotePeers = remotePeers,
          peerHandlerFlow
        )
      }
      peerChangesTopic <- Resource.eval(p2pServer.peerChanges)
      _ <- Async[F].background(
        peerChangesTopic.subscribeUnbounded
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
      peerClients = peerChangesTopic.subscribeUnbounded.collect {
        case PeerConnectionChanges.ConnectionEstablished(_, client) => client
      }
      _ <- handleNetworkClients(peerClients, clientHandler)
      _ <- Resource.eval(Logger[F].info(s"Bound P2P at host=$host port=$bindPort"))
    } yield p2pServer

  private def handleNetworkClients[F[_]: Async: Parallel: Logger](
    clients:       Stream[F, BlockchainPeerClient[F]],
    clientHandler: BlockchainPeerHandlerAlgebra[F]
  ) =
    Async[F]
      .background(
        clients
          .evalMap(client =>
            Async[F].start(
              clientHandler
                .usePeer(client)
                .handleErrorWith(t =>
                  client.remotePeer
                    .flatMap(peer => Logger[F].error(t)(show"Client connection to remote=${peer.remoteAddress} failed"))
                    .void
                )
            )
          )
          .compile
          .toList
          .flatMap(_.parTraverse(_.joinWithUnit))
      )

}
