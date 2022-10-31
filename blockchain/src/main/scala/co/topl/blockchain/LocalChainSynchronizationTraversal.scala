package co.topl.blockchain

import akka.stream.Materializer
import akka.stream.scaladsl.Sink
import cats.effect.Async
import co.topl.algebras.{SynchronizationTraversal, SynchronizationTraversalStep, SynchronizationTraversalSteps}
import co.topl.catsakka.SourceMatNotUsed
import co.topl.eventtree.ParentChildTree
import co.topl.models.TypedIdentifier
import fs2.{Chunk, Pipe, Pull, Stream}
import fs2.interop.reactivestreams._

/**
 * Transform a stream of local block adoptions into a stream of head traversal steps. The input stream
 * only contains block IDs that are adopted; it does not indicate the steps that were taken to reach that point from the
 * previous block adoption. The adoption may take place in an entirely different tine, so this traversal takes care of
 * indicating which blocks should be unapplied, and which blocks should be applied
 */
object LocalChainSynchronizationTraversal {

  def make[F[_]: Async](
    currentHead:     TypedIdentifier,
    adoptionsStream: SourceMatNotUsed[TypedIdentifier],
    parentChildTree: ParentChildTree[F, TypedIdentifier]
  )(implicit
    m: Materializer
  ): SynchronizationTraversal[F, Stream[F, SynchronizationTraversalStep]] = {

    val pullSteps: Pipe[F, TypedIdentifier, SynchronizationTraversalStep] = {
      def go(s: Stream[F, TypedIdentifier], currentHead: TypedIdentifier): Pull[F, SynchronizationTraversalStep, Unit] =
        s.pull.uncons.flatMap {
          case Some((chunk, tlStream)) =>
            chunk.head match {
              case Some(newHead) =>
                Pull
                  .eval(parentChildTree.findCommonAncestor(currentHead, newHead))
                  .map { case (unapplyChain, applyChain) =>
                    unapplyChain.tail.map(SynchronizationTraversalSteps.Unapplied) ++
                    applyChain.tail.map(SynchronizationTraversalSteps.Applied)
                  }
                  .map(Chunk.chain)
                  .flatMap(steps =>
                    Pull.output(steps) >>
                    go(Stream.chunk(chunk.drop(1)) ++ tlStream, newHead)
                  )
              case None =>
                Pull.done
            }
          case None =>
            Pull.done
        }
      in => go(in, currentHead).stream
    }

    new SynchronizationTraversal[F, Stream[F, SynchronizationTraversalStep]] {
      def headChanges: F[Stream[F, SynchronizationTraversalStep]] =
        Async[F].delay {
          adoptionsStream
            .runWith(Sink.asPublisher[TypedIdentifier](fanout = false))
            .toStreamBuffered(bufferSize = 1)
            .through(pullSteps)
        }
    }
  }
}
