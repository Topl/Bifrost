package co.topl.blockchain

import cats.{Monad, Show}
import cats.data.EitherT
import cats.implicits._
import cats.effect.Async
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.algebras.{Store, ToplRpc}
import co.topl.catsakka.FToFuture
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.eventtree.EventSourcedState
import co.topl.ledger.algebras.{MempoolAlgebra, TransactionSyntaxValidationAlgebra}
import co.topl.ledger.models._
import co.topl.models.{BlockBodyV2, BlockHeaderV2, Transaction, TypedIdentifier}
import org.typelevel.log4cats.Logger
import co.topl.typeclasses.implicits._

object ToplRpcServer {

  implicit private val showTransactionSyntaxError: Show[TransactionSyntaxError] = {
    case TransactionSyntaxErrors.EmptyInputs                 => "EmptyInputs"
    case TransactionSyntaxErrors.DuplicateInput(boxId)       => show"DuplicateInput(boxId=$boxId)"
    case TransactionSyntaxErrors.ExcessiveOutputsCount       => "ExcessiveOutputsCount"
    case TransactionSyntaxErrors.InvalidTimestamp(timestamp) => show"InvalidTimestamp(timestamp=$timestamp)"
    case TransactionSyntaxErrors.NonPositiveOutputValue(outputValue) =>
      show"NonPositiveOutputValue(value=${outputValue.toString})"
    case TransactionSyntaxErrors.InsufficientInputFunds(_, _) => "InsufficientInputFunds"
    case TransactionSyntaxErrors.InvalidProofType(_, _)       => "InvalidProofType"

  }

  implicit private val showTransactionSemanticError: Show[TransactionSemanticError] = {
    case TransactionSemanticErrors.InputDataMismatch(input) => show"InputDataMismatch(boxId=${input.boxId})"
    case TransactionSemanticErrors.UnspendableBox(boxId)    => show"UnspendableBox(boxId=$boxId)"
    case TransactionSemanticErrors.UnsatisfiedSchedule(slot, schedule) =>
      show"UnsatisfiedSchedule(slot=$slot, minimumSlot=${schedule.minimumSlot}, maximumSlot=${schedule.maximumSlot})"
  }

  /**
   * Interpreter which serves Topl RPC data using local blockchain interpreters
   */
  def make[F[_]: Async: Logger: FToFuture](
    headerStore:         Store[F, TypedIdentifier, BlockHeaderV2],
    bodyStore:           Store[F, TypedIdentifier, BlockBodyV2],
    transactionStore:    Store[F, TypedIdentifier, Transaction],
    mempool:             MempoolAlgebra[F],
    syntacticValidation: TransactionSyntaxValidationAlgebra[F],
    localChain:          LocalChainAlgebra[F],
    blockHeights:        EventSourcedState[F, Long => F[Option[TypedIdentifier]]]
  ): F[ToplRpc[F]] =
    Async[F].delay {
      new ToplRpc[F] {

        def broadcastTransaction(transaction: Transaction): F[Unit] =
          transactionStore
            .contains(transaction.id)
            .ifM(
              Logger[F].info(show"Received duplicate transaction id=${transaction.id.asTypedBytes}"),
              Logger[F].info(show"Received RPC Transaction id=${transaction.id.asTypedBytes}") >>
              syntacticValidateOrRaise(transaction)
                .flatTap(_ =>
                  Logger[F].info(show"Transaction id=${transaction.id.asTypedBytes} is syntactically valid")
                )
                .flatTap(processValidTransaction[F](transactionStore, mempool))
                .void
            )

        def currentMempool(): F[Set[TypedIdentifier]] =
          localChain.head.map(_.slotId.blockId).flatMap(mempool.read)

        def fetchBlockHeader(blockId: TypedIdentifier): F[Option[BlockHeaderV2]] =
          headerStore.get(blockId)

        def fetchBlockBody(blockId: TypedIdentifier): F[Option[BlockBodyV2]] =
          bodyStore.get(blockId)

        def fetchTransaction(transactionId: TypedIdentifier): F[Option[Transaction]] =
          transactionStore.get(transactionId)

        def blockIdAtHeight(height: Long): F[Option[TypedIdentifier]] =
          localChain.head.map(_.slotId.blockId).flatMap(blockHeights.useStateAt(_)(_.apply(height)))

        private def syntacticValidateOrRaise(transaction: Transaction) =
          EitherT(syntacticValidation.validate(transaction).map(_.toEither))
            .leftSemiflatTap(errors =>
              Logger[F].warn(
                show"Received syntactically invalid transaction id=${transaction.id.asTypedBytes} reasons=$errors"
              )
            )
            .leftMap(_ =>
              new IllegalArgumentException(s"Syntactically invalid transaction id=${transaction.id.asTypedBytes}")
            )
            .rethrowT
      }
    }

  private def processValidTransaction[F[_]: Monad: Logger](
    transactionStore: Store[F, TypedIdentifier, Transaction],
    mempool:          MempoolAlgebra[F]
  )(transaction:      Transaction) =
    Logger[F].info(show"Inserting Transaction id=${transaction.id.asTypedBytes} into transaction store") >>
    transactionStore.put(transaction.id, transaction) >>
    Logger[F].info(show"Inserting Transaction id=${transaction.id.asTypedBytes} into mempool") >>
    mempool.add(transaction.id)

}
