package co.topl.blockchain

import akka.stream.Materializer
import akka.stream.scaladsl.Source
import cats.data.Validated
import cats.effect.kernel.Async
import cats.effect.std.Queue
import cats.implicits._
import co.topl.catsakka._
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.models.{SlotData, TypedIdentifier}
import fs2.Stream

object LocalChainBroadcaster {

  /**
   * A LocalChain interpreter which wraps a delegate LocalChain. Any blocks adopted by this LocalChain will be announced in the returned Source
   * @param localChain a delegate interpreter
   * @return a tuple (interpreter, adoptionsSource, adoptionsStream)
   */
  def make[F[_]: Async](
    localChain: LocalChainAlgebra[F]
  )(implicit
    materializer: Materializer
  ): F[
    (LocalChainAlgebra[F], SourceMatNotUsed[TypedIdentifier], Stream[F, TypedIdentifier], Stream[F, TypedIdentifier])
  ] =
    (
      Async[F].delay(Source.backpressuredQueue[F, TypedIdentifier]().preMaterialize()),
      Queue.bounded[F, TypedIdentifier](capacity = 16), // ToplRpcServer
      // TODO unused stream, changes in BN-690-Re-imlementLocalChainBroadCaster_v2
      Queue.circularBuffer[F, TypedIdentifier](capacity = 1) // BlockchainPeerServer
    ).mapN { case (((offer, _), source), rpcServerBoundedQueue, p2pServerBoundedQueue) =>
      val interpreter = new LocalChainAlgebra[F] {
        def isWorseThan(newHead: SlotData): F[Boolean] = localChain.isWorseThan(newHead)

        def adopt(newHead: Validated.Valid[SlotData]): F[Unit] =
          localChain.adopt(newHead) >>
          offer(newHead.a.slotId.blockId) >>
          rpcServerBoundedQueue.offer(newHead.a.slotId.blockId) >>
          p2pServerBoundedQueue.offer(newHead.a.slotId.blockId)

        def head: F[SlotData] = localChain.head
      }
      (
        interpreter,
        source,
        Stream.fromQueueUnterminated(rpcServerBoundedQueue),
        Stream.fromQueueUnterminated(p2pServerBoundedQueue)
      )
    }

}
