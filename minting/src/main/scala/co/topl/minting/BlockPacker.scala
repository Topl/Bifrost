package co.topl.minting

import cats.Monad
import cats.data.EitherT
import cats.effect.std.Queue
import cats.effect.{Async, Sync}
import cats.implicits._
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.ledger.algebras._
import co.topl.minting.algebras.BlockPackerAlgebra
import co.topl.models._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.immutable.ListSet

/**
 * An extremely naive and greedy implementation of the BlockPackerAlgebra.
 */
object BlockPacker {

  def make[F[_]: Async](
    mempool:          MempoolAlgebra[F],
    fetchTransaction: TypedIdentifier => F[Transaction],
    validateBody:     (TypedIdentifier, BlockBodyV2) => F[Boolean]
  ): F[BlockPackerAlgebra[F]] = {
    implicit val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromClass[F](BlockPacker.getClass)
    Sync[F].delay((parentBlockId: TypedIdentifier) =>
      for {
        // Read all transaction IDs from the mempool
        mempoolTransactionIds <- mempool.read(parentBlockId)
        _                     <- Logger[F].debug(show"Block packing candidates=${mempoolTransactionIds.toList}")
        transactions          <- mempoolTransactionIds.toList.traverse(fetchTransaction)
        iterative             <-
          // Enqueue all of the transactions (in no particular order, which is terrible for performance and accuracy)
          Queue
            .unbounded[F, Transaction]
            .flatTap(queue => transactions.traverse(queue.offer))
            .map(queue =>
              new Iterative[F, BlockBodyV2.Full] {

                def improve(current: BlockBodyV2.Full): F[BlockBodyV2.Full] =
                  // Dequeue the next transaction (or block forever)
                  queue.take
                    .flatMap { transaction =>
                      // Attempt to stuff that transaction into our current block
                      val fullBody = current.append(transaction)
                      val body = ListSet.empty[TypedIdentifier] ++ fullBody.toList.map(_.id.asTypedBytes)
                      // If it's valid, hooray.  If not, return the previous value
                      validateBody(parentBlockId, body)
                        .ifF(fullBody, current)
                    }
                    .logDuration("BlockPacker Iteration")
              }
            )
      } yield iterative
    )
  }

  def makeBodyValidator[F[_]: Monad: Logger](
    bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:      BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F]
  ): (TypedIdentifier, BlockBodyV2) => F[Boolean] =
    (parentId, proposedBody) =>
      (
        EitherT(bodySyntaxValidation.validate(proposedBody).map(_.toEither)).leftMap(_.toString) >>
        EitherT(bodySemanticValidation.validate(parentId)(proposedBody).map(_.toEither)).leftMap(_.toString) >>
        EitherT(bodyAuthorizationValidation.validate(parentId)(proposedBody).map(_.toEither)).leftMap(_.toString)
      )
        .leftSemiflatTap(error => Logger[F].debug(show"Block packer candidate is invalid.  reason=$error"))
        .isRight
}
