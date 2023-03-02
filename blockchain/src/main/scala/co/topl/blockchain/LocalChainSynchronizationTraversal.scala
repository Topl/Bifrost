package co.topl.blockchain

import cats.effect.Async
import co.topl.algebras.SynchronizationTraversal
import co.topl.algebras.SynchronizationTraversalStep
import co.topl.algebras.SynchronizationTraversalSteps
import co.topl.consensus.models.BlockId
import co.topl.eventtree.ParentChildTree
import fs2.Chunk
import fs2.Pipe
import fs2.Pull
import fs2.Stream

/**
 * Transform a stream of local block adoptions into a stream of head traversal steps. The input stream
 * only contains block IDs that are adopted; it does not indicate the steps that were taken to reach that point from the
 * previous block adoption. The adoption may take place in an entirely different tine, so this traversal takes care of
 * indicating which blocks should be unapplied, and which blocks should be applied
 */
object LocalChainSynchronizationTraversal {

  def make[F[_]: Async](
    currentHead:     BlockId,
    adoptionsStream: Stream[F, BlockId],
    parentChildTree: ParentChildTree[F, BlockId]
  ): SynchronizationTraversal[F, Stream[F, *]] = {

    val pullSteps: Pipe[F, BlockId, SynchronizationTraversalStep] = {
      def go(s: Stream[F, BlockId], currentHead: BlockId): Pull[F, SynchronizationTraversalStep, Unit] =
        s.pull.uncons1.flatMap {
          case Some((head, tlStream)) =>
            Pull
              .eval(parentChildTree.findCommonAncestor(currentHead, head))
              .map { case (unapplyChain, applyChain) =>
                unapplyChain.tail.map(SynchronizationTraversalSteps.Unapplied) ++
                applyChain.tail.map(SynchronizationTraversalSteps.Applied)
              }
              .map(Chunk.chain)
              .flatMap(steps => Pull.output(steps) >> go(tlStream, head))
          case None =>
            Pull.done
        }
      in => go(in, currentHead).stream
    }

    new SynchronizationTraversal[F, Stream[F, *]] {
      override def headChanges: F[Stream[F, SynchronizationTraversalStep]] =
        Async[F].delay(
          adoptionsStream.through(pullSteps)
        )
    }
  }
}
