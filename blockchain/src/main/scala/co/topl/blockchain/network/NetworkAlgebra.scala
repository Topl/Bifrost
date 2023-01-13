package co.topl.blockchain.network

import cats.effect.{Resource, Temporal}
import cats.effect.kernel.Concurrent
import co.topl.blockchain.network.BlockBodiesChecker.BlockBodiesCheckerActor
import co.topl.blockchain.network.BlockHeadersChecker.BlockHeadersCheckerActor
import co.topl.blockchain.network.PeerActor.PeerActor
import co.topl.blockchain.network.PeersManager.PeersManagerActor
import co.topl.blockchain.network.ReputationAggregator.ReputationAggregatorActor

trait NetworkAlgebra[F[_]] {

  def makePeerManger(
    networkAlgebra:            NetworkAlgebra[F],
    reputationAggregatorActor: ReputationAggregatorActor[F],
    blockHeadersChecker: BlockHeadersCheckerActor[F],
    blockBodiesChecker: BlockBodiesCheckerActor[F]
  ): Resource[F, PeersManagerActor[F]]

  def makePeer(
    hostId:               HostId,
    reputationAggregator: ReputationAggregatorActor[F],
    blockHeaderChecker:   BlockHeadersCheckerActor[F],
    blockBodiesChecker:   BlockBodiesCheckerActor[F]
  ): Resource[F, PeerActor[F]]
  def makeReputationAggregation: Resource[F, ReputationAggregatorActor[F]]

  def makeBlockHeaderChecker(
    reputationAggregator: ReputationAggregatorActor[F],
    blockBodyFetcher:     BlockBodiesCheckerActor[F]
  ): Resource[F, BlockHeadersCheckerActor[F]]

  def makeBlockBodyChecker(
    reputationAggregatorActor: ReputationAggregatorActor[F]
  ): Resource[F, BlockBodiesCheckerActor[F]]
}

class NetworkAlgebraImpl[F[_]: Concurrent: Temporal] extends NetworkAlgebra[F] {

  override def makePeerManger(
    networkAlgebra:            NetworkAlgebra[F],
    reputationAggregatorActor: ReputationAggregatorActor[F],
    blockHeadersChecker: BlockHeadersCheckerActor[F],
    blockBodiesChecker: BlockBodiesCheckerActor[F]
  ): Resource[F, PeersManagerActor[F]] =
    PeersManager.makeActor(networkAlgebra, reputationAggregatorActor, blockHeadersChecker, blockBodiesChecker)

  override def makePeer(
    hostId:               HostId,
    reputationAggregator: ReputationAggregatorActor[F],
    blockHeaderChecker:   BlockHeadersCheckerActor[F],
    blockBodiesChecker:   BlockBodiesCheckerActor[F]
  ): Resource[F, PeerActor[F]] =
    PeerActor.makeActor(hostId, reputationAggregator, blockHeaderChecker, blockBodiesChecker)

  override def makeReputationAggregation: Resource[F, ReputationAggregatorActor[F]] = ReputationAggregator.makeActor

  override def makeBlockHeaderChecker(
    reputationAggregator: ReputationAggregatorActor[F],
    blockBodyChecker:     BlockBodiesCheckerActor[F]
  ): Resource[F, BlockHeadersCheckerActor[F]] =
    BlockHeadersChecker.makeActor(reputationAggregator, blockBodyChecker)

  override def makeBlockBodyChecker(
                            reputationAggregatorActor: ReputationAggregatorActor[F]
                          ): Resource[F, BlockBodiesCheckerActor[F]] =
    BlockBodiesChecker.makeActor(reputationAggregatorActor)
}
