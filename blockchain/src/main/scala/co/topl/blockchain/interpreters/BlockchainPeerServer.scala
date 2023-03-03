package co.topl.blockchain.interpreters

import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.brambl.models.Identifier
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.catsakka.DroppingTopic
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.consensus.models.BlockId
import co.topl.eventtree.EventSourcedState
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.consensus.models.{BlockHeader, SlotData}
import co.topl.node.models.BlockBody
import co.topl.typeclasses.implicits._
import co.topl.networking.blockchain.BlockchainPeerServerAlgebra
import co.topl.networking.p2p.ConnectedPeer
import fs2.Stream
import fs2.concurrent.Topic
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object BlockchainPeerServer {

  def make[F[_]: Async](
    fetchSlotData:           BlockId => F[Option[SlotData]],
    fetchHeader:             BlockId => F[Option[BlockHeader]],
    fetchBody:               BlockId => F[Option[BlockBody]],
    fetchTransaction:        Identifier.IoTransaction32 => F[Option[IoTransaction]],
    blockHeights:            EventSourcedState[F, Long => F[Option[BlockId]], BlockId],
    localChain:              LocalChainAlgebra[F],
    mempool:                 MempoolAlgebra[F],
    newBlockIds:             Topic[F, BlockId],
    newTransactionIds:       Topic[F, Identifier.IoTransaction32],
    blockIdBufferSize:       Int = 8,
    transactionIdBufferSize: Int = 64
  )(peer: ConnectedPeer): Resource[F, BlockchainPeerServerAlgebra[F]] =
    (
      DroppingTopic(newBlockIds, blockIdBufferSize).flatMap(_.subscribeAwaitUnbounded),
      DroppingTopic(newTransactionIds, transactionIdBufferSize).flatMap(_.subscribeAwaitUnbounded)
    )
      .mapN((newBlockIds, newTransactionIds) =>
        new BlockchainPeerServerAlgebra[F] {

          implicit private val logger: Logger[F] =
            Slf4jLogger
              .getLoggerFromName[F]("P2P.BlockchainPeerServer")
              .withModifiedString(value => show"peer=${peer.remoteAddress} $value")

          /**
           * Serves a stream containing the current head ID plus a stream of block IDs adopted in the local chain.
           */
          def localBlockAdoptions: F[Stream[F, BlockId]] =
            Async[F].delay(
              Stream
                .eval(localChain.head.map(_.slotId.blockId))
                .append(newBlockIds)
                .evalTap(id => Logger[F].debug(show"Broadcasting block id=$id to peer"))
            )

          /**
           * Serves a stream containing all _current_ mempool transactions plus a stream containing
           * any new mempool transaction as-it-happens
           */
          def localTransactionNotifications: F[Stream[F, Identifier.IoTransaction32]] =
            Async[F].delay(
              Stream
                .eval(localChain.head.map(_.slotId.blockId).flatMap(mempool.read))
                .flatMap(Stream.iterable)
                .append(newTransactionIds)
                .evalTap(id => Logger[F].debug(show"Broadcasting transaction id=$id to peer"))
            )

          def getLocalSlotData(id: BlockId): F[Option[SlotData]] =
            fetchSlotData(id)

          def getLocalHeader(id: BlockId): F[Option[BlockHeader]] =
            fetchHeader(id)

          def getLocalBody(id: BlockId): F[Option[BlockBody]] =
            fetchBody(id)

          def getLocalTransaction(id: Identifier.IoTransaction32): F[Option[IoTransaction]] =
            fetchTransaction(id)

          def getLocalBlockAtHeight(height: Long): F[Option[BlockId]] =
            for {
              head       <- localChain.head
              blockIdOpt <- blockHeights.useStateAt(head.slotId.blockId)(_.apply(height))
            } yield blockIdOpt
        }
      )
}
