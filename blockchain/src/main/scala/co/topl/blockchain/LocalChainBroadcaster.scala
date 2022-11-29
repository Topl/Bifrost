package co.topl.blockchain

import akka.stream.Materializer
import akka.stream.scaladsl.Source
import cats.data.Validated
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.catsakka._
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.models.{SlotData, TypedIdentifier}
import fs2.concurrent.Topic

object LocalChainBroadcaster {

  /**
   * A LocalChain interpreter which wraps a delegate LocalChain.  Any blocks adopted by
   * this LocalChain will be announced in the returned Source
   * @param localChain a delegate interpreter
   * @return a tuple (interpreter, adoptionsSource)
   */
  def make[F[_]: Async](
    localChain:            LocalChainAlgebra[F],
    adoptionsTopic:        Topic[F, TypedIdentifier]
  )(implicit materializer: Materializer): F[(LocalChainAlgebra[F], SourceMatNotUsed[TypedIdentifier])] =
    Async[F]
      .delay(Source.backpressuredQueue[F, TypedIdentifier]().preMaterialize())
      .map { case ((offer, _), source) =>
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
            localChain.adopt(newHead) >> offer(newHead.a.slotId.blockId) >>
            adoptionsTopic.publish1(newHead.a.slotId.blockId).map(_.leftMap(_ => ())).map(_.merge)

          def head: F[SlotData] = localChain.head
        }

        (interpreter, source)
      }

}
