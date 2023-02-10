package co.topl.blockchain

import cats.{Monad, Show}
import cats.data.EitherT
import cats.implicits._
import cats.effect.Async
import cats.effect.kernel.Sync
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.algebras.{Store, SynchronizationTraversalStep, ToplRpc}
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras.{MempoolAlgebra, TransactionSyntaxValidationAlgebra}
import co.topl.ledger.models._
import co.topl.{models => legacyModels}
import legacyModels.{Transaction => LTransaction, TypedIdentifier}
import legacyModels.utility._
import co.topl.node.models.BlockBody
import co.topl.consensus.models.BlockHeader
import co.topl.proto.models.Transaction
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}
import co.topl.typeclasses.implicits._
import fs2.Stream
import org.typelevel.log4cats.slf4j.Slf4jLogger

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
  def make[F[_]: Async](
    headerStore:               Store[F, TypedIdentifier, BlockHeader],
    bodyStore:                 Store[F, TypedIdentifier, BlockBody],
    transactionStore:          Store[F, TypedIdentifier, LTransaction], // TODO change Transaction to new Model
    mempool:                   MempoolAlgebra[F],
    syntacticValidation:       TransactionSyntaxValidationAlgebra[F],
    localChain:                LocalChainAlgebra[F],
    blockHeights:              EventSourcedState[F, Long => F[Option[TypedIdentifier]], TypedIdentifier],
    blockIdTree:               ParentChildTree[F, TypedIdentifier],
    localBlockAdoptionsStream: Stream[F, TypedIdentifier]
  ): F[ToplRpc[F, Stream[F, *]]] =
    Async[F].delay {
      new ToplRpc[F, Stream[F, *]] {
        implicit private val logger: SelfAwareStructuredLogger[F] =
          Slf4jLogger.getLoggerFromName[F]("Bifrost.RPC.Server")

        def broadcastTransaction(transaction: Transaction): F[Unit] =
          // TODO model should change to new protobuf specs and not use Isomorphism
          EitherT(
            co.topl.models.utility
              .transactionIsomorphism[F]
              .baMorphism
              .aToB(Sync[F].delay(transaction))
          )
            .leftMap(new IllegalArgumentException(_))
            .rethrowT
            .flatMap { transaction =>
              val id = transaction.id.asTypedBytes
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

        def currentMempool(): F[Set[TypedIdentifier]] =
          localChain.head.map(_.slotId.blockId).flatMap(blockId => mempool.read(blockId: TypedIdentifier))

        def fetchBlockHeader(blockId: TypedIdentifier): F[Option[BlockHeader]] =
          headerStore.get(blockId)

        def fetchBlockBody(blockId: TypedIdentifier): F[Option[BlockBody]] =
          bodyStore.get(blockId)

        def fetchTransaction(transactionId: TypedIdentifier): F[Option[Transaction]] =
          transactionStore
            .get(transactionId)
            .map(transaction =>
              co.topl.models.utility
                .transactionIsomorphism[Option]
                // TODO model should change to new protobuf specs and not use Isomorphism, change the store
                .abMorphism
                .aToB(transaction)
                .flatMap(_.toOption)
            )

        def blockIdAtHeight(height: Long): F[Option[TypedIdentifier]] =
          for {
            _    <- Async[F].raiseWhen(height < 1)(new IllegalArgumentException("Invalid height"))
            head <- localChain.head
            atHeight <-
              if (head.height === height) (head.slotId.blockId: TypedIdentifier).some.pure[F]
              else if (head.height < height) none.pure[F]
              else blockHeights.useStateAt(head.slotId.blockId)(_.apply(height))
          } yield atHeight

        def blockIdAtDepth(depth: Long): F[Option[TypedIdentifier]] =
          for {
            _    <- Async[F].raiseWhen(depth < 0)(new IllegalArgumentException("Negative depth"))
            head <- localChain.head
            atDepth <-
              if (depth === 0L) (head.slotId.blockId: TypedIdentifier).some.pure[F]
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

        private def syntacticValidateOrRaise(transaction: LTransaction) =
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
    transactionStore: Store[F, TypedIdentifier, LTransaction],
    mempool:          MempoolAlgebra[F]
  )(transaction:      LTransaction) =
    Logger[F].debug(show"Inserting Transaction id=${transaction.id.asTypedBytes} into transaction store") >>
    transactionStore.put(transaction.id, transaction) >>
    Logger[F].debug(show"Inserting Transaction id=${transaction.id.asTypedBytes} into mempool") >>
    mempool.add(transaction.id) >>
    Logger[F].info(show"Processed Transaction id=${transaction.id.asTypedBytes} from RPC")

}
