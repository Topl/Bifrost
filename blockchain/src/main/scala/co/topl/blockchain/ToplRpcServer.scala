package co.topl.blockchain

import cats.{Monad, Show}
import cats.data.EitherT
import cats.implicits._
import cats.effect.Async
import co.topl.codecs.bytes.tetra.instances._
import co.topl.algebras.{Store, SynchronizationTraversalStep, ToplRpc}
import co.topl.brambl.models.Identifier
import co.topl.brambl.validation.TransactionSyntaxError
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.node.models.BlockBody
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.BlockId
import co.topl.models.Transaction
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}
import co.topl.typeclasses.implicits._
import fs2.Stream
import org.typelevel.log4cats.slf4j.Slf4jLogger

object ToplRpcServer {

  implicit private val showTransactionSyntaxError: Show[TransactionSyntaxError] = {
    case TransactionSyntaxError.EmptyInputs                 => "EmptyInputs"
    case TransactionSyntaxError.DuplicateInput(boxId)       => show"DuplicateInput(boxId=$boxId)"
    case TransactionSyntaxError.ExcessiveOutputsCount       => "ExcessiveOutputsCount"
    case TransactionSyntaxError.InvalidTimestamp(timestamp) => show"InvalidTimestamp(timestamp=$timestamp)"
    case TransactionSyntaxError.NonPositiveOutputValue(outputValue) =>
      show"NonPositiveOutputValue(value=${outputValue.toString})"
    case TransactionSyntaxError.InsufficientInputFunds(_, _) => "InsufficientInputFunds"
    case TransactionSyntaxError.InvalidProofType(_, _)       => "InvalidProofType"
    case TransactionSyntaxError.InvalidSchedule(s) =>
      show"InvalidSchedule(creation=${s.timestamp},maximumSlot=${s.max},minimumSlot=${s.min})"
    case TransactionSyntaxError.InvalidDataLength => "InvalidDataLength"
  }

  /**
   * Interpreter which serves Topl RPC data using local blockchain interpreters
   */
  def make[F[_]: Async](
    headerStore:               Store[F, BlockId, BlockHeader],
    bodyStore:                 Store[F, BlockId, BlockBody],
    transactionStore:          Store[F, Identifier.IoTransaction32, Transaction],
    mempool:                   MempoolAlgebra[F],
    syntacticValidation:       TransactionSyntaxVerifier[F],
    localChain:                LocalChainAlgebra[F],
    blockHeights:              EventSourcedState[F, Long => F[Option[BlockId]], BlockId],
    blockIdTree:               ParentChildTree[F, BlockId],
    localBlockAdoptionsStream: Stream[F, BlockId]
  ): F[ToplRpc[F, Stream[F, *]]] =
    Async[F].delay {
      new ToplRpc[F, Stream[F, *]] {
        implicit private val logger: SelfAwareStructuredLogger[F] =
          Slf4jLogger.getLoggerFromName[F]("Bifrost.RPC.Server")

        def broadcastTransaction(transaction: Transaction): F[Unit] = {
          val id = transaction.id
          transactionStore
            .contains(id)
            .ifM(
              Logger[F].info(show"Received duplicate transaction id=$id"),
              Logger[F].debug(show"Received RPC Transaction id=$id") >>
              syntacticValidateOrRaise(transaction)
                .flatTap(_ => Logger[F].debug(show"Transaction id=$id is syntactically valid"))
                .flatTap(processValidTransaction[F](transactionStore, mempool))
                .void
            )
        }

        def currentMempool(): F[Set[Identifier.IoTransaction32]] =
          localChain.head.map(_.slotId.blockId).flatMap(blockId => mempool.read(blockId))

        def fetchBlockHeader(blockId: BlockId): F[Option[BlockHeader]] =
          headerStore.get(blockId)

        def fetchBlockBody(blockId: BlockId): F[Option[BlockBody]] =
          bodyStore.get(blockId)

        def fetchTransaction(transactionId: Identifier.IoTransaction32): F[Option[Transaction]] =
          transactionStore
            .get(transactionId)

        def blockIdAtHeight(height: Long): F[Option[BlockId]] =
          for {
            _    <- Async[F].raiseWhen(height < 1)(new IllegalArgumentException("Invalid height"))
            head <- localChain.head
            atHeight <-
              if (head.height === height) head.slotId.blockId.some.pure[F]
              else if (head.height < height) none.pure[F]
              else blockHeights.useStateAt(head.slotId.blockId)(_.apply(height))
          } yield atHeight

        def blockIdAtDepth(depth: Long): F[Option[BlockId]] =
          for {
            _    <- Async[F].raiseWhen(depth < 0)(new IllegalArgumentException("Negative depth"))
            head <- localChain.head
            atDepth <-
              if (depth === 0L) head.slotId.blockId.some.pure[F]
              else if (depth > head.height) none.pure[F]
              else blockHeights.useStateAt(head.slotId.blockId)(_.apply(head.height - depth))
          } yield atDepth

        def synchronizationTraversal(): F[Stream[F, SynchronizationTraversalStep]] =
          localChain.head
            .map(_.slotId.blockId)
            .flatMap { currentHead =>
              LocalChainSynchronizationTraversal
                .make[F](
                  currentHead,
                  localBlockAdoptionsStream,
                  blockIdTree
                )
                .headChanges
            }

        private def syntacticValidateOrRaise(transaction: Transaction) =
          EitherT(syntacticValidation.validate(transaction))
            .leftSemiflatTap(errors =>
              Logger[F].warn(
                show"Received syntactically invalid transaction id=${transaction.id} reasons=$errors"
              )
            )
            .leftMap(errors =>
              new IllegalArgumentException(
                show"Syntactically invalid transaction" +
                show" id=${transaction.id}" +
                show" reasons=$errors"
              )
            )
            .rethrowT
      }
    }

  private def processValidTransaction[F[_]: Monad: Logger](
    transactionStore: Store[F, Identifier.IoTransaction32, Transaction],
    mempool:          MempoolAlgebra[F]
  )(transaction: Transaction) =
    Logger[F].debug(show"Inserting Transaction id=${transaction.id} into transaction store") >>
    transactionStore.put(transaction.id, transaction) >>
    Logger[F].debug(show"Inserting Transaction id=${transaction.id} into mempool") >>
    mempool.add(transaction.id) >>
    Logger[F].info(show"Processed Transaction id=${transaction.id} from RPC")

}
