package co.topl.consensus.interpreters

import cats.Applicative
import cats.data.ValidatedNec
import cats.implicits._
import co.topl.consensus.algebras.TransactionConsensusValidationAlgebra
import co.topl.consensus.models.TransactionConsensusError
import co.topl.models.{Transaction, TypedIdentifier}

/**
 * A contextual transaction validator for the purposes of Consensus.
 *
 * NOTE: At this time, this always returns a success.  Once delegation is supported, we need to validate certain
 * aspects of the transaction.
 *
 * TODO: What to do if a transaction's address delegates to a non-existent registration?  Reject?
 * Or is that a necessary use-case to allow for pre-delegating of funds before registering?
 */
object TransactionConsensusValidation {

  def make[F[_]: Applicative](): F[TransactionConsensusValidationAlgebra[F]] =
    Applicative[F].pure(
      new TransactionConsensusValidationAlgebra[F] {

        def validate(context: TypedIdentifier)(
          t:                  Transaction
        ): F[ValidatedNec[TransactionConsensusError, Transaction]] =
          t.validNec[TransactionConsensusError].pure[F]
      }
    )
}
