package co.topl.networking.blockchain

import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.blockchain.BlockchainCore
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.catsutils._
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.networking.fsnetwork.RemotePeer
import co.topl.networking.p2p.PeerConnectionChanges.RemotePeerApplicationLevel
import co.topl.networking.p2p.{ConnectedPeer, PeerConnectionChange}
import co.topl.node.models._
import co.topl.typeclasses.implicits._
import fs2.Stream
import fs2.concurrent.Topic
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object BlockchainPeerServer {

  // scalastyle:off method.length
  def make[F[_]: Async](
    blockchain:              BlockchainCore[F],
    peerServerPort:          () => Option[KnownHost],
    currentHotPeers:         () => F[Set[RemotePeer]],
    peerStatus:              Topic[F, PeerConnectionChange],
    blockIdBufferSize:       Int = 8,
    transactionIdBufferSize: Int = 512
  )(peer: ConnectedPeer): Resource[F, BlockchainPeerServerAlgebra[F]] =
    (
      Resource.pure[F, Stream[F, BlockId]](
        Stream.force(blockchain.consensus.localChain.adoptions).dropOldest(blockIdBufferSize)
      ),
      DroppingTopic(blockchain.ledger.mempool.adoptions, transactionIdBufferSize).flatMap(_.subscribeAwaitUnbounded)
    )
      .mapN((newBlockIds, newTransactionIds) =>
        new BlockchainPeerServerAlgebra[F] {

          implicit private val logger: Logger[F] =
            Slf4jLogger
              .getLoggerFromName[F]("P2P.BlockchainPeerServer")
              .withModifiedString(value => show"peer=${peer.remoteAddress} $value")

          override def peerAsServer: F[Option[KnownHost]] = peerServerPort().pure[F]

          /**
           * Serves a stream containing the current head ID plus a stream of block IDs adopted in the local chain.
           */
          override def localBlockAdoptions: F[Stream[F, BlockId]] =
            Async[F].delay(
              Stream
                .eval(blockchain.consensus.localChain.head.map(_.slotId.blockId))
                .append(newBlockIds)
                .evalTap(id => Logger[F].debug(show"Broadcasting block id=$id to peer"))
            )

          /**
           * Serves a stream containing all _current_ mempool transactions plus a stream containing
           * any new mempool transaction as-it-happens
           */
          override def localTransactionNotifications: F[Stream[F, TransactionId]] =
            Async[F].delay(
              Stream
                .eval(
                  blockchain.consensus.localChain.head.map(_.slotId.blockId).flatMap(blockchain.ledger.mempool.read)
                )
                .flatMap(g => Stream.iterable(g.transactions.keys))
                .append(newTransactionIds)
                .evalTap(id => Logger[F].debug(show"Broadcasting transaction id=$id to peer"))
            )

          override def getLocalSlotData(id: BlockId): F[Option[SlotData]] =
            blockchain.dataStores.slotData.get(id)

          override def getLocalHeader(id: BlockId): F[Option[BlockHeader]] =
            blockchain.dataStores.headers.get(id)

          override def getLocalBody(id: BlockId): F[Option[BlockBody]] =
            blockchain.dataStores.bodies.get(id)

          override def getLocalTransaction(id: TransactionId): F[Option[IoTransaction]] =
            blockchain.dataStores.transactions.get(id)

          override def getLocalBlockAtHeight(height: Long): F[Option[BlockId]] =
            blockchain.consensus.localChain.blockIdAtHeight(height)

          override def getLocalBlockAtDepth(depth: Long): F[Option[BlockId]] =
            blockchain.consensus.localChain.head.flatMap(s =>
              blockchain.consensus.localChain.blockIdAtHeight(s.height - depth)
            )

          override def getKnownHosts(req: CurrentKnownHostsReq): F[Option[CurrentKnownHostsRes]] =
            for {
              remotePeers <- currentHotPeers().map(_.toSeq.take(req.maxCount))
              knownHosts = remotePeers.map(rp => KnownHost(rp.peerId.id, rp.address.host, rp.address.port))
            } yield Option(CurrentKnownHostsRes(knownHosts, Seq.empty, Seq.empty))

          override def getPong(req: PingMessage): F[Option[PongMessage]] =
            Option(PongMessage(req.ping.reverse)).pure[F]

          override def notifyApplicationLevel(isEnabled: Boolean): F[Option[Unit]] =
            peerStatus.publish1(RemotePeerApplicationLevel(peer, isEnabled)).map(_ => Option(()))
        }
      )

  // scalastyle:on method.length
}
