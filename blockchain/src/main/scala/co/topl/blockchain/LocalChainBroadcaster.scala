package co.topl.blockchain

import cats.data.{EitherT, Validated}
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.consensus.models.SlotData
import co.topl.models.TypedIdentifier
import co.topl.models.utility._
import fs2.concurrent.Topic

object LocalChainBroadcaster {

  /**
   * A LocalChain interpreter which wraps a delegate LocalChain.  Any blocks adopted by
   * this LocalChain will be announced in the returned Source
   * @param localChain a delegate interpreter
   * @return a tuple (interpreter, adoptionsSource)
   */
  def make[F[_]: Async](
    localChain: LocalChainAlgebra[F]
  ): Resource[F, (LocalChainAlgebra[F], Topic[F, TypedIdentifier])] =
    Resource
      .make(Topic[F, TypedIdentifier])(_.close.void)
      .map { topic =>
        val interpreter = new LocalChainAlgebra[F] {
          def isWorseThan(newHead: SlotData): F[Boolean] = localChain.isWorseThan(newHead)

          /**
           * TODO adoptionsTopic.publish1:
           * This operation does not complete until after the given element has been enqued on all subscribers,
           * which means that if any subscriber is at its maxQueued limit,
           * publish1 will semantically block until that subscriber consumes an element.
           * Check Blockchain:toplRpcAdoptionConsumer: maxQueued
           */
          def adopt(newHead: Validated.Valid[SlotData]): F[Unit] =
            localChain.adopt(newHead) >>
            EitherT(topic.publish1(newHead.a.slotId.blockId: TypedIdentifier))
              .leftMap(_ => new IllegalStateException("LocalChainBroadcaster topic unexpectedly closed"))
              .rethrowT

          def head: F[SlotData] = localChain.head
        }

        (interpreter, topic)
      }

}
