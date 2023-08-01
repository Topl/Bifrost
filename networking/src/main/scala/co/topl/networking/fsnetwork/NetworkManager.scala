package co.topl.networking.fsnetwork

import cats.data.NonEmptyChain
import cats.effect.kernel.Resource
import cats.implicits._
import co.topl.algebras.{ClockAlgebra, Store}
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.config.ApplicationConfig.Bifrost.NetworkProperties
import co.topl.consensus.algebras._
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.algebras._
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.node.models.BlockBody
import org.typelevel.log4cats.Logger

object NetworkManager {

  def startNetwork[F[_]: Logger](
    localChain:                  LocalChainAlgebra[F],
    chainSelectionAlgebra:       ChainSelectionAlgebra[F, SlotData],
    headerValidation:            BlockHeaderValidationAlgebra[F],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:      BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F],
    slotDataStore:               Store[F, BlockId, SlotData],
    headerStore:                 Store[F, BlockId, BlockHeader],
    bodyStore:                   Store[F, BlockId, BlockBody],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    blockIdTree:                 ParentChildTree[F, BlockId],
    networkAlgebra:              NetworkAlgebra[F],
    initialHosts:                List[HostId],
    networkProperties:           NetworkProperties,
    clock:                       ClockAlgebra[F],
    addRemotePeerAlgebra:        PeerCreationRequestAlgebra[F]
  ): Resource[F, PeersManagerActor[F]] =
    for {
      _            <- Resource.liftK(Logger[F].info(s"Start actors network with list of known peers: $initialHosts"))
      slotDuration <- Resource.liftK(clock.slotLength)
      p2pNetworkConfig = P2PNetworkConfig(networkProperties, slotDuration)

      peerManager <- networkAlgebra.makePeerManger(
        networkAlgebra,
        localChain,
        slotDataStore,
        transactionStore,
        blockIdTree,
        headerToBodyValidation,
        addRemotePeerAlgebra,
        p2pNetworkConfig
      )

      reputationAggregator <- networkAlgebra.makeReputationAggregation(peerManager, p2pNetworkConfig)

      requestsProxy <- networkAlgebra.makeRequestsProxy(reputationAggregator, peerManager, headerStore, bodyStore)
      blocksChecker <- networkAlgebra.makeBlockChecker(
        reputationAggregator,
        requestsProxy,
        localChain,
        slotDataStore,
        headerStore,
        bodyStore,
        headerValidation,
        bodySyntaxValidation,
        bodySemanticValidation,
        bodyAuthorizationValidation,
        chainSelectionAlgebra
      )

      _ <- Resource.liftK(requestsProxy.sendNoWait(RequestsProxy.Message.SetupBlockChecker(blocksChecker)))
      _ <- Resource.liftK(peerManager.sendNoWait(PeersManager.Message.SetupReputationAggregator(reputationAggregator)))
      _ <- Resource.liftK(peerManager.sendNoWait(PeersManager.Message.SetupBlockChecker(blocksChecker)))
      _ <- Resource.liftK(peerManager.sendNoWait(PeersManager.Message.SetupRequestsProxy(requestsProxy)))

      _ <- Resource.liftK(
        NonEmptyChain
          .fromSeq(initialHosts)
          .map { initialPeers =>
            peerManager.sendNoWait(PeersManager.Message.AddKnownPeers(initialPeers))
          }
          .getOrElse(Logger[F].error(show"No know hosts are set during node startup"))
      )

      notifier <- networkAlgebra.makeNotifier(peerManager, reputationAggregator, p2pNetworkConfig)
      _        <- Resource.liftK(notifier.sendNoWait(Notifier.Message.StartNotifications))
    } yield peerManager
}
