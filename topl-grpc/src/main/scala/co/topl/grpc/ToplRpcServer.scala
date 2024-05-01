package co.topl.grpc

import cats.data.{EitherT, OptionT}
import cats.effect.Async
import cats.implicits._
import cats.{Monad, Show}
import co.topl.algebras._
import co.topl.blockchain.{BlockchainCore, LocalChainSynchronizationTraversal}
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax._
import co.topl.brambl.validation.TransactionSyntaxError
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.models.{BlockHeader, BlockId}
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.models.Epoch
import co.topl.node.models.BlockBody
import co.topl.proto.node.{EpochData, NodeConfig}
import co.topl.typeclasses.implicits._
import fs2.Stream
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}

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
    case TransactionSyntaxError.InvalidDataLength        => "InvalidDataLength"
    case TransactionSyntaxError.InvalidUpdateProposal(_) => "InvalidUpdateProposal"
  }

  // scalastyle:off method.length
  /**
   * Interpreter which serves Topl RPC data using local blockchain interpreters
   */
  def make[F[_]: Async](blockchain: BlockchainCore[F]): F[NodeRpc[F, Stream[F, *]]] =
    Async[F].delay {
      new NodeRpc[F, Stream[F, *]] {

        implicit private val logger: SelfAwareStructuredLogger[F] =
          Slf4jLogger.getLoggerFromName[F]("Bifrost.RPC.Server")

        def broadcastTransaction(transaction: IoTransaction): F[Unit] =
          Async[F]
            .delay(transaction.embedId)
            .flatMap { transaction =>
              val id = transaction.id
              blockchain.dataStores.transactions
                .contains(id)
                .ifM(
                  Logger[F].info(show"Received duplicate transaction id=$id"),
                  Logger[F].debug(show"Received RPC Transaction id=$id") >>
                  syntacticValidateOrRaise(transaction)
                    .flatTap(_ => Logger[F].debug(show"Transaction id=$id is syntactically valid"))
                    .flatTap(processValidTransaction[F](blockchain.dataStores.transactions, blockchain.ledger.mempool))
                    .void
                )
            }

        def currentMempool(): F[Set[TransactionId]] =
          blockchain.consensus.localChain.head
            .map(_.slotId.blockId)
            .flatMap(blockId => blockchain.ledger.mempool.read(blockId))
            .map(_.transactions.keySet)

        override def currentMempoolContains(transactionId: TransactionId): F[Boolean] =
          blockchain.consensus.localChain.head
            .map(_.slotId.blockId)
            .flatMap(blockId => blockchain.ledger.mempool.contains(blockId, transactionId))

        def fetchBlockHeader(blockId: BlockId): F[Option[BlockHeader]] =
          OptionT(blockchain.dataStores.headers.get(blockId)).map(_.embedId).value

        def fetchBlockBody(blockId: BlockId): F[Option[BlockBody]] =
          blockchain.dataStores.bodies.get(blockId)

        def fetchTransaction(transactionId: TransactionId): F[Option[IoTransaction]] =
          OptionT(blockchain.dataStores.transactions.get(transactionId)).map(_.embedId).value

        def blockIdAtHeight(height: Long): F[Option[BlockId]] =
          for {
            _    <- Async[F].raiseWhen(height < 1)(new IllegalArgumentException("Invalid height"))
            head <- blockchain.consensus.localChain.head
            atHeight <-
              if (head.height === height) head.slotId.blockId.some.pure[F]
              else if (head.height < height) none.pure[F]
              else blockchain.consensus.localChain.blockIdAtHeight(height)
          } yield atHeight

        def blockIdAtDepth(depth: Long): F[Option[BlockId]] =
          for {
            _    <- Async[F].raiseWhen(depth < 0)(new IllegalArgumentException("Negative depth"))
            head <- blockchain.consensus.localChain.head
            atDepth <-
              if (depth === 0L) head.slotId.blockId.some.pure[F]
              else if (depth >= head.height) none.pure[F]
              else blockchain.consensus.localChain.blockIdAtHeight(head.height - depth)
          } yield atDepth

        def synchronizationTraversal(): F[Stream[F, SynchronizationTraversalStep]] =
          blockchain.consensus.localChain.head
            .map(_.slotId.blockId)
            .flatMap { currentHead =>
              LocalChainSynchronizationTraversal
                .make[F](
                  currentHead,
                  Stream.force(blockchain.consensus.localChain.adoptions),
                  blockchain.blockIdTree
                )
                .headChanges
            }

        def fetchProtocolConfigs(): F[Stream[F, NodeConfig]] =
          blockchain.protocolConfiguration.fetchNodeConfig

        def fetchEpochData(epoch: Option[Epoch]): F[Option[EpochData]] =
          OptionT
            .fromOption[F](epoch)
            .getOrElseF(blockchain.clock.currentEpoch)
            .flatMap(blockchain.epochData.dataOf)

        private def syntacticValidateOrRaise(transaction: IoTransaction) =
          EitherT(blockchain.validators.transactionSyntax.validate(transaction))
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
  // scalastyle:on method.length

  private def processValidTransaction[F[_]: Monad: Logger](
    transactionStore: Store[F, TransactionId, IoTransaction],
    mempool:          MempoolAlgebra[F]
  )(transaction: IoTransaction) =
    for {
      id  <- transaction.id.pure[F]
      _   <- Logger[F].debug(show"Inserting Transaction id=$id into transaction store")
      _   <- transactionStore.put(id, transaction)
      _   <- Logger[F].debug(show"Inserting Transaction id=$id into mempool")
      res <- mempool.add(id)
      _   <- Logger[F].info(show"Processed Transaction id=$id from RPC, added to memory pool is $res")
    } yield ()

}
