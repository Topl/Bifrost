package co.topl.ledger.interpreters

import cats.data.{EitherT, NonEmptyChain, Validated, ValidatedNec}
import cats.effect.Sync
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import co.topl.node.models.BlockBody

object BodySemanticValidation {

  def make[F[_]: Sync](
    fetchTransaction:              TransactionId => F[IoTransaction],
    transactionSemanticValidation: TransactionSemanticValidationAlgebra[F],
    registrationAccumulator:       RegistrationAccumulatorAlgebra[F]
  ): F[BodySemanticValidationAlgebra[F]] =
    Sync[F].delay {
      new BodySemanticValidationAlgebra[F] {

        /**
         * Semantically validates each of the transactions in the given block.  The given transactions _may_ spend
         * the outputs of previous transactions in the block, but no two transactions may spend the same input.
         */
        def validate(context: BodyValidationContext)(body: BlockBody): F[ValidatedNec[BodySemanticError, BlockBody]] =
          body.transactionIds
            .foldLeftM(List.empty[IoTransaction].validNec[BodySemanticError]) {
              case (Validated.Valid(prefix), transactionId) =>
                validateTransaction(context, prefix)(transactionId).map(prefix :+ _).value.map(_.toValidated)
              case (invalid, _) => invalid.pure[F]
            }
            .map(_.as(body))

        /**
         * Fetch the given transaction ID and (semantically) validate it in the context of the given block ID.
         * Transaction semantic validation uses the given augmented box state during validation.
         *
         * @return a ValidatedNec containing either errors or the original Transaction
         */
        private def validateTransaction(
          context: BodyValidationContext,
          prefix:  Seq[IoTransaction]
        )(transactionId: TransactionId) =
          for {
            transaction <- EitherT.liftF(fetchTransaction(transactionId))
            transactionValidationContext = StaticTransactionValidationContext(
              context.parentHeaderId,
              prefix,
              context.height,
              context.slot
            )
            _ <-
              EitherT(transactionSemanticValidation.validate(transactionValidationContext)(transaction).map(_.toEither))
                .leftMap(errors =>
                  NonEmptyChain[BodySemanticError](BodySemanticErrors.TransactionSemanticErrors(transaction, errors))
                )
            augmentedRegistrationAccumulator = RegistrationAccumulator.Augmented.make(registrationAccumulator)(
              prefix.foldLeft(RegistrationAccumulator.Augmentation.empty)(_.augment(_))
            )
            newRegistrations = transaction.outputs
              .flatMap(_.value.value.topl.flatMap(_.registration).map(_.address))
              .toSet -- transaction.inputs.flatMap(_.value.value.topl.flatMap(_.registration).map(_.address))
            _ <- EitherT
              .liftF(
                augmentedRegistrationAccumulator.use(accumulator =>
                  newRegistrations.toList.forallM(accumulator.contains(context.parentHeaderId)(_).map(!_))
                )
              )
              .flatMap(
                EitherT.cond(
                  _,
                  (),
                  NonEmptyChain[BodySemanticError](BodySemanticErrors.TransactionRegistrationError(transaction))
                )
              )
          } yield transaction
      }
    }

}
