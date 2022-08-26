package co.topl.minting

import cats.Monad
import cats.data.{Chain, OptionT}
import cats.effect.{Async, Sync}
import cats.implicits._
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.ledger.algebras._
import co.topl.minting.algebras.BlockPackerAlgebra
import co.topl.models._
import co.topl.typeclasses.implicits._

import scala.collection.immutable.ListSet

object BlockPacker {

  def make[F[_]: Async](
    mempool:          MempoolAlgebra[F],
    fetchTransaction: TypedIdentifier => F[Transaction],
    validateBody:     (TypedIdentifier, BlockBodyV2) => F[Boolean]
  ): F[BlockPackerAlgebra[F]] =
    Sync[F].delay((parentBlockId: TypedIdentifier) =>
      for {
        mempoolTransactionIds <- mempool.read(parentBlockId)
        transactions          <- mempoolTransactionIds.toList.traverse(fetchTransaction)
        iterative: Iterative[F, BlockBodyV2.Full] =
          if (transactions.isEmpty) {
            new Iterative[F, BlockBodyV2.Full] {
              private var emitted = false
              def improve(current: BlockBodyV2.Full): F[BlockBodyV2.Full] =
                if (emitted) Async[F].never[BlockBodyV2.Full]
                else {
                  emitted = true
                  Chain.empty[Transaction].pure[F]
                }
            }
          } else {
            val remainingTransactions = scala.collection.mutable.Queue.from(transactions)
            (selected: BlockBodyV2.Full) =>
              if (remainingTransactions.isEmpty) Async[F].never
              else {
                val transaction = remainingTransactions.dequeue()
                val fullBody = transaction +: selected
                val body = ListSet.empty[TypedIdentifier] ++ fullBody.toList.map(_.id.asTypedBytes)
                validateBody(parentBlockId, body).ifF(fullBody, selected)
              }
          }
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
