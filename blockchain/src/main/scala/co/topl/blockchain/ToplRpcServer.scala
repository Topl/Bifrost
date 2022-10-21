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
    case TransactionSyntaxErrors.InvalidSchedule(s) =>
      show"InvalidSchedule(creation=${s.creation},maximumSlot=${s.maximumSlot},minimumSlot=${s.minimumSlot})"
    case TransactionSyntaxErrors.InvalidDataLength => "InvalidDataLength"
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
                  Logger[F].debug(show"Transaction id=${transaction.id.asTypedBytes} is syntactically valid")
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
          for {
            _    <- Async[F].raiseWhen(height < 1)(new IllegalArgumentException("Invalid height"))
            head <- localChain.head
            atHeight <-
              if (head.height === height) head.slotId.blockId.some.pure[F]
              else if (head.height < height) none.pure[F]
              else blockHeights.useStateAt(head.slotId.blockId)(_.apply(height))
          } yield atHeight

        def blockIdAtDepth(depth: Long): F[Option[TypedIdentifier]] =
          for {
            _    <- Async[F].raiseWhen(depth < 0)(new IllegalArgumentException("Negative depth"))
            head <- localChain.head
            atDepth <-
              if (depth === 0L) head.slotId.blockId.some.pure[F]
              else if (depth > head.height) none.pure[F]
              else blockHeights.useStateAt(head.slotId.blockId)(_.apply(head.height - depth))
          } yield atDepth

        private def syntacticValidateOrRaise(transaction: Transaction) =
          EitherT(syntacticValidation.validate(transaction).map(_.toEither))
            .leftSemiflatTap(errors =>
              Logger[F].warn(
                show"Received syntactically invalid transaction id=${transaction.id.asTypedBytes} reasons=$errors"
              )
            )
            .leftMap(errors =>
              new IllegalArgumentException(
                show"Syntactically invalid transaction" +
                show" id=${transaction.id.asTypedBytes}" +
                show" reasons=$errors"
              )
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
