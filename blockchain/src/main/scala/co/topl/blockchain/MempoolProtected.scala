package co.topl.blockchain

import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.Async
import co.topl.brambl.models.TransactionId
import co.topl.consensus.models.{BlockHeader, BlockId}
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import co.topl.ledger.implicits._
import cats.implicits._
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import cats.effect.implicits._
import org.typelevel.log4cats.Logger
import co.topl.typeclasses.implicits._
import co.topl.brambl.validation.algebras.TransactionAuthorizationVerifier
import co.topl.ledger.interpreters.QuivrContext
import cats.effect.std.Semaphore
import co.topl.config.ApplicationConfig.Bifrost.MempoolProtection
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps

object MempoolProtected {

  def make[F[_]: Async: Logger](
    mempool:                          MempoolAlgebra[F],
    semanticValidationAlgebra:        TransactionSemanticValidationAlgebra[F],
    transactionAuthorizationVerifier: TransactionAuthorizationVerifier[F],
    currentBlockHeader:               F[BlockHeader],
    fetchTransaction:                 TransactionId => F[IoTransaction],
    transactionRewardCalculator:      TransactionRewardCalculatorAlgebra,
    config:                           MempoolProtection
  ): Resource[F, MempoolAlgebra[F]] =
    for {
      semaphore <- Semaphore[F](1).toResource
      _         <- Logger[F].info(s"Create protected mempool with parameters: $config").toResource
      memoryPool <- Async[F]
        .delay(
          new MempoolProtected(
            mempool,
            semanticValidationAlgebra,
            transactionAuthorizationVerifier,
            currentBlockHeader,
            fetchTransaction,
            transactionRewardCalculator,
            config,
            semaphore
          )
        )
        .toResource
    } yield memoryPool

  class MempoolProtected[F[_]: Async: Logger](
    underlying:               MempoolAlgebra[F],
    semanticValidation:       TransactionSemanticValidationAlgebra[F],
    transactionAuthorization: TransactionAuthorizationVerifier[F],
    currentBlockHeader:       F[BlockHeader],
    fetchTransaction:         TransactionId => F[IoTransaction],
    txRewardCalculator:       TransactionRewardCalculatorAlgebra,
    config:                   MempoolProtection,
    semaphore:                Semaphore[F]
  ) extends MempoolAlgebra[F] {
    override def read(blockId: BlockId): F[MempoolGraph] = underlying.read(blockId)

    override def add(transactionId: TransactionId): F[Boolean] = semaphore.permit.use { _ =>
      for {
        header      <- currentBlockHeader
        graph       <- read(header.id)
        transaction <- fetchTransaction(transactionId).map(tx => IoTransactionEx(tx, txRewardCalculator.rewardsOf(tx)))
        res         <- checkTransaction(header, graph, transaction)
        _ <-
          if (res) {
            underlying
              .add(transactionId)
              .flatMap(r => Logger[F].info(show"Added tx $transactionId to mempool with res $r"))
          } else {
            ().pure[F]
          }
      } yield res
    }

    override def remove(transactionId: TransactionId): F[Unit] =
      Logger[F].info(show"Received request to remove $transactionId") >> underlying.remove(transactionId)

    override def contains(blockId: BlockId, transactionId: TransactionId): F[Boolean] =
      underlying.contains(blockId, transactionId)

    private def checkTransaction(
      currentHeader: BlockHeader,
      graph:         MempoolGraph,
      transaction:   IoTransactionEx
    ): F[Boolean] = {
      val txId = transaction.tx.id

      if (!config.enabled || (graph.memSize + transaction.size) < config.protectionEnabledThreshold) {
        Logger[F].debug(show"Skip tx verification $txId") >>
        true.pure[F]
      } else {
        EitherT
          .right[String](transaction.pure[F])
          .flatMap(doSemanticCheck(currentHeader, graph))
          .flatMap(doAuthCheck(currentHeader))
          .flatMap(doFeeCheck(graph))
          .value
          .recoverWith { case e =>
            Logger[F].debug(e)(show"Error during checking transaction in Mempool") >>
            Either.left[String, IoTransactionEx](e.toString).pure[F]
          }
          .flatMap {
            _.fold(
              e => Logger[F].info(show"Skip adding transaction to mempool $txId because of $e") >> false.pure[F],
              _ => Logger[F].debug(show"Transaction $txId could be added to mempool") >> true.pure[F]
            )
          }
      }
    }

    private def doSemanticCheck(currentHeader: BlockHeader, graph: MempoolGraph)(
      txToValidate: IoTransactionEx
    ): EitherT[F, String, IoTransactionEx] = {
      val semanticContextTemplate = StaticTransactionValidationContext(
        currentHeader.id,
        Seq.empty,
        currentHeader.height,
        currentHeader.slot
      )

      val semanticContext =
        if ((graph.memSize + txToValidate.size) < config.useMempoolForSemanticThreshold)
          semanticContextTemplate.copy(prefix = graph.ioTransactions.values.toSeq)
        else semanticContextTemplate

      EitherT(
        semanticValidation
          .validate(semanticContext)(txToValidate.tx)
          .map(_.toEither.bimap(_.mkString_(","), _ => txToValidate))
      )
    }

    private def doAuthCheck(
      currentHeader: BlockHeader
    )(txToValidate: IoTransactionEx): EitherT[F, String, IoTransactionEx] = {
      val authContext = QuivrContext.forProposedBlock(currentHeader.height, currentHeader.slot, txToValidate.tx)
      EitherT(
        transactionAuthorization
          .validate(authContext)(txToValidate.tx)
          .map(_.bimap(e => show"$e", _ => txToValidate))
      )
    }

    private def doFeeCheck(
      graph: MempoolGraph
    )(txToValidate: IoTransactionEx): EitherT[F, String, IoTransactionEx] =
      if ((graph.memSize + txToValidate.size) < config.feeFilterThreshold) {
        EitherT.right[String](txToValidate.pure[F])
      } else {
        val meanFeePerKByte = Math.max(1, graph.meanToplFeePerKByte)
        val freeMempoolSize = Math.max(0, config.maxMempoolSize - graph.memSize)
        val freeMempoolSizePercent = freeMempoolSize / config.maxMempoolSize
        val minimumFeePerKByte =
          if (freeMempoolSizePercent == 0) Double.MaxValue
          else (meanFeePerKByte / freeMempoolSizePercent) - meanFeePerKByte

        Either
          .cond(
            test = txToValidate.toplFeePerKByte >= minimumFeePerKByte,
            right = txToValidate,
            left = show"Transaction ${txToValidate.tx.id} have fee ${txToValidate.toplFeePerKByte} per Kb " +
              show"but minimum acceptable fee is $minimumFeePerKByte per Kb"
          )
          .toEitherT[F]
      }
  }
}
