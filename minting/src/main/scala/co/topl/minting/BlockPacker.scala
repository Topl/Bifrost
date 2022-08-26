package co.topl.minting

import cats.Monad
import cats.data.OptionT
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
import org.typelevel.log4cats.Logger

import scala.collection.immutable.ListSet

object BlockPacker {

  def make[F[_]: Async: Logger](
    mempool:          MempoolAlgebra[F],
    fetchTransaction: TypedIdentifier => F[Transaction],
    validateBody:     (TypedIdentifier, BlockBodyV2) => F[Boolean]
  ): F[BlockPackerAlgebra[F]] =
    Sync[F].delay((parentBlockId: TypedIdentifier) =>
      for {
        mempoolTransactionIds <- mempool.read(parentBlockId)
        _                     <- Logger[F].info(s"Considering mempool transaction IDs=${mempoolTransactionIds.toList}")
        transactions          <- mempoolTransactionIds.toList.traverse(fetchTransaction)
        iterative <-
          Queue
            .unbounded[F, Transaction]
            .flatTap(queue => transactions.traverse(queue.offer))
            .map(queue =>
              new Iterative[F, BlockBodyV2.Full] {

                def improve(current: BlockBodyV2.Full): F[BlockBodyV2.Full] =
                  queue.take.flatMap { transaction =>
                    val fullBody = transaction +: current
                    val body = ListSet.empty[TypedIdentifier] ++ fullBody.toList.map(_.id.asTypedBytes)
                    validateBody(parentBlockId, body).ifF(fullBody, current)
                  }
              }
            )
      } yield iterative
    )

  def makeBodyValidator[F[_]: Monad](
    bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:      BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F]
  ): (TypedIdentifier, BlockBodyV2) => F[Boolean] =
    (parentId, proposedBody) =>
      (
        OptionT(bodySyntaxValidation.validate(proposedBody).map(_.toOption)) >>
        OptionT(bodySemanticValidation.validate(parentId)(proposedBody).map(_.toOption)) >>
        OptionT(bodyAuthorizationValidation.validate(parentId)(proposedBody).map(_.toOption))
      ).isDefined
}
