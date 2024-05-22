package co.topl.networking.fsnetwork

import cats.Applicative
import cats.data.NonEmptyChain
import cats.effect.Async
import cats.effect.implicits.genSpawnOps
import cats.effect.kernel.{Outcome, Resource}
import cats.implicits._
import co.topl.algebras.Store
import co.topl.blockchain.BlockchainCore
import co.topl.config.ApplicationConfig.Bifrost.NetworkProperties
import co.topl.models.p2p.{HostId, KnownRemotePeer}
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.p2p.{DisconnectedPeer, PeerConnectionChange, PeerConnectionChanges}
import co.topl.networking.fsnetwork.P2PShowInstances._
import com.google.protobuf.ByteString
import fs2.concurrent.Topic
import org.typelevel.log4cats.Logger

import scala.util.Random

object NetworkManager {

  def startNetwork[F[_]: Async: Logger](
    thisHostId:              HostId,
    blockchain:              BlockchainCore[F],
    networkAlgebra:          NetworkAlgebra[F],
    initialHosts:            Seq[DisconnectedPeer],
    networkProperties:       NetworkProperties,
    addRemotePeerAlgebra:    PeerCreationRequestAlgebra[F],
    peersStatusChangesTopic: Topic[F, PeerConnectionChange],
    hotPeersUpdate:          Set[RemotePeer] => F[Unit]
  ): Resource[F, PeersManagerActor[F]] =
    for {
      _ <- Resource.liftK(Logger[F].info(show"Start actors network with list of peers: ${initialHosts.mkString(";")}"))
      slotDuration <- Resource.liftK(blockchain.clock.slotLength)
      p2pNetworkConfig = P2PNetworkConfig(networkProperties, slotDuration)

      peersFromStorage <- Resource.liftK(blockchain.dataStores.knownHosts.get(()).map(_.getOrElse(Seq.empty)))
      _                <- Resource.liftK(Logger[F].info(show"Loaded from storage next known hosts: $peersFromStorage"))
      peerManager <- networkAlgebra.makePeerManger(
        thisHostId,
        networkAlgebra,
        blockchain.consensus.localChain,
        blockchain.consensus.chainSelection,
        blockchain.dataStores.slotData,
        blockchain.dataStores.bodies,
        blockchain.dataStores.transactions,
        blockchain.blockIdTree,
        blockchain.ledger.mempool,
        blockchain.validators.headerToBody,
        blockchain.validators.transactionSyntax,
        addRemotePeerAlgebra,
        p2pNetworkConfig,
        hotPeersUpdate,
        buildSaveRemotePeersFunction(blockchain.dataStores.knownHosts)
      )

      requestsProxy <- networkAlgebra.makeRequestsProxy(
        peerManager,
        blockchain.dataStores.headers,
        blockchain.dataStores.bodies
      )
      blocksChecker <- networkAlgebra.makeBlockChecker(
        requestsProxy,
        blockchain.consensus.localChain,
        blockchain.dataStores.slotData,
        blockchain.dataStores.headers,
        blockchain.dataStores.bodies,
        blockchain.validators.header,
        blockchain.validators.bodySyntax,
        blockchain.validators.bodySemantics,
        blockchain.validators.bodyAuthorization,
        blockchain.consensus.chainSelection,
        p2pNetworkConfig
      )

      _ <- Resource.liftK(requestsProxy.sendNoWait(RequestsProxy.Message.SetupBlockChecker(blocksChecker)))
      _ <- Resource.liftK(peerManager.sendNoWait(PeersManager.Message.SetupBlockChecker(blocksChecker)))
      _ <- Resource.liftK(peerManager.sendNoWait(PeersManager.Message.SetupRequestsProxy(requestsProxy)))

      _ <- Resource.liftK(
        NonEmptyChain
          .fromSeq(mergeRemotePeersAndRemoteAddress(peersFromStorage, initialHosts))
          .map { initialPeers =>
            peerManager.sendNoWait(PeersManager.Message.AddKnownPeers(initialPeers))
          }
          .getOrElse(Logger[F].error(show"No know hosts are set during node startup"))
      )

      notifier <- networkAlgebra.makeNotifier(peerManager, p2pNetworkConfig)
      _        <- Resource.liftK(notifier.sendNoWait(Notifier.Message.StartNotifications))

      _ <- startPeersStatusNotifier(peerManager, peersStatusChangesTopic)
    } yield peerManager

  private def startPeersStatusNotifier[F[_]: Async: Logger](
    peersManager: PeersManagerActor[F],
    topic:        Topic[F, PeerConnectionChange]
  ): Resource[F, F[Outcome[F, Throwable, Unit]]] =
    topic.subscribeUnbounded
      .evalTap {
        case PeerConnectionChanges.InboundConnectionInitializing(_, _) => Applicative[F].unit
        case PeerConnectionChanges.OutboundConnectionInitializing(_)   => Applicative[F].unit
        case PeerConnectionChanges.ConnectionEstablished(_, localAddress) =>
          peersManager.sendNoWait(PeersManager.Message.UpdateThisPeerAddress(localAddress))
        case PeerConnectionChanges.ConnectionClosed(connectedPeer, _) =>
          Logger[F].info(show"Remote peer ${connectedPeer.remoteAddress} closing had been detected") >>
          connectedPeer.p2pVK
            .map(b => HostId(b))
            .traverse_(id => peersManager.sendNoWait(PeersManager.Message.ClosePeer(id)))
        case PeerConnectionChanges.RemotePeerApplicationLevel(connectedPeer, appLevel) =>
          peersManager.sendNoWait(PeersManager.Message.RemotePeerNetworkLevel(HostId(connectedPeer.p2pVK), appLevel))
        case PeerConnectionChanges.ChangedRemotePeer(oldPeer, newPeer) =>
          val oldAndNewIds = for {
            oldId <- oldPeer.p2pVK
            newId <- newPeer.p2pVK
          } yield (HostId(oldId), HostId(newId))
          oldAndNewIds.traverse_ { case (oldId, newId) =>
            Logger[F].info(show"Peer $oldId changed to $newId") >>
            peersManager.sendNoWait(PeersManager.Message.RemotePeerIdChanged(oldId, newId))
          }
      }
      .compile
      .drain
      .background

  private def buildSaveRemotePeersFunction[F[_]: Async: Logger](
    remotePeersStore: Store[F, Unit, Seq[KnownRemotePeer]]
  ): Set[KnownRemotePeer] => F[Unit] = { peers: Set[KnownRemotePeer] =>
    Logger[F].info(show"Going to save known hosts $peers to local data storage") >>
    remotePeersStore.put((), peers.toList)
  }

  // peers represented as Remote address could be present in remote peers as well with some reputation
  private def mergeRemotePeersAndRemoteAddress(
    remotePeers:   Seq[KnownRemotePeer],
    remoteAddress: Seq[DisconnectedPeer]
  ): Seq[KnownRemotePeer] = {
    val remoteAddressMap = remoteAddress.map { ra =>
      val id =
        ra.p2pVK.map(HostId).getOrElse(HostId(ByteString.copyFrom(Random.nextBytes(hostIdBytesLen))))
      ra.remoteAddress -> KnownRemotePeer(id, ra.remoteAddress, 0, 0, None)
    }.toMap
    val remotePeersMap = remotePeers.map(p => p.address -> p).toMap

    (remoteAddressMap ++ remotePeersMap).values.toSeq
  }
}
