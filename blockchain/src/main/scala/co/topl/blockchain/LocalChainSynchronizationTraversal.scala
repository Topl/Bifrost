package co.topl.blockchain

import cats.effect.Async
import co.topl.algebras.{SynchronizationTraversal, SynchronizationTraversalStep, SynchronizationTraversalSteps}
import co.topl.eventtree.ParentChildTree
import co.topl.models.TypedIdentifier
import fs2.{Chunk, Pipe, Pull, Stream}

/**
 * Transform a stream of local block adoptions into a stream of head traversal steps. The input stream
 * only contains block IDs that are adopted; it does not indicate the steps that were taken to reach that point from the
 * previous block adoption. The adoption may take place in an entirely different tine, so this traversal takes care of
 * indicating which blocks should be unapplied, and which blocks should be applied
 */
object LocalChainSynchronizationTraversal {

  def make[F[_]: Async](
    currentHead:     TypedIdentifier,
    adoptionsStream: Stream[F, TypedIdentifier],
    parentChildTree: ParentChildTree[F, TypedIdentifier]
  ): SynchronizationTraversal[F, Stream[F, *]] = {

    val pullSteps: Pipe[F, TypedIdentifier, SynchronizationTraversalStep] = {
      def go(s: Stream[F, TypedIdentifier], currentHead: TypedIdentifier): Pull[F, SynchronizationTraversalStep, Unit] =
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
