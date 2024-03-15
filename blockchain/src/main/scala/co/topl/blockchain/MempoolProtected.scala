package co.topl.blockchain

import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.Async
import co.topl.brambl.models.TransactionId
import co.topl.consensus.models.{BlockHeader, BlockId}
import co.topl.ledger.algebras.{MempoolAlgebra, TransactionSemanticValidationAlgebra}
import co.topl.ledger.models._
import cats.implicits._
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import cats.effect.implicits._
import org.typelevel.log4cats.Logger
import co.topl.typeclasses.implicits._
import co.topl.brambl.validation.algebras.TransactionAuthorizationVerifier
import co.topl.ledger.interpreters.QuivrContext
import cats.Show
import cats.effect.std.Semaphore
import co.topl.brambl.validation.TransactionAuthorizationError
import co.topl.config.ApplicationConfig.Bifrost.MempoolProtection

object MempoolProtected {

  def make[F[_]: Async: Logger](
    mempool:                          MempoolAlgebra[F],
    semanticValidationAlgebra:        TransactionSemanticValidationAlgebra[F],
    transactionAuthorizationVerifier: TransactionAuthorizationVerifier[F],
    currentBlockHeader:               F[BlockHeader],
    fetchTransaction:                 TransactionId => F[IoTransaction],
    config:                           MempoolProtection
  ): Resource[F, MempoolAlgebra[F]] =
    for {
      semaphore <- Semaphore[F](1).toResource
      memoryPool <- Async[F]
        .delay(
          new MempoolProtected(
            mempool,
            semanticValidationAlgebra,
            transactionAuthorizationVerifier,
            currentBlockHeader,
            fetchTransaction,
            config,
            semaphore
          )
        )
        .toResource
    } yield memoryPool

  class MempoolProtected[F[_]: Async: Logger](
    underlying:                       MempoolAlgebra[F],
    semanticValidationAlgebra:        TransactionSemanticValidationAlgebra[F],
    transactionAuthorizationVerifier: TransactionAuthorizationVerifier[F],
    currentBlockHeader:               F[BlockHeader],
    fetchTransaction:                 TransactionId => F[IoTransaction],
    config:                           MempoolProtection,
    semaphore:                        Semaphore[F]
  ) extends MempoolAlgebra[F] {
    override def read(blockId: BlockId): F[MempoolGraph] = underlying.read(blockId)

    override def add(transactionId: TransactionId): F[Boolean] = semaphore.permit.use { _ =>
      for {
        currentHeader <- currentBlockHeader
        mempoolGraph  <- read(currentHeader.id)
        txMetadata    <- MempoolMetadata(mempoolGraph).pure[F]
        res           <- checkTransaction(currentHeader, mempoolGraph, txMetadata, transactionId)
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
      meta:          MempoolMetadata,
      txId:          TransactionId
    ): F[Boolean] =
      if (!config.enabled || meta.txCount < config.noCheckIfLess) {
        Logger[F].debug(show"Skip tx verification: ${meta.txCount} txs in mempool") >>
        true.pure[F]
      } else {
        EitherT
          .right[String](fetchTransaction(txId))
          .flatMap(doSemanticCheck(currentHeader, graph, meta))
          .flatMap(doAuthCheck(currentHeader))
          .value
          .recoverWith { case e =>
            Logger[F].debug(e)(show"Error during checking transaction in Mempool") >>
            Either.left[String, IoTransaction](e.toString).pure[F]
          }
          .flatMap {
            _.fold(
              e => Logger[F].info(show"Skip adding transaction to mempool $txId because of $e") >> false.pure[F],
              _ => Logger[F].debug(show"Transaction $txId could be added to mempool") >> true.pure[F]
            )
          }
      }

    implicit val semanticError: Show[TransactionSemanticError] = (e: TransactionSemanticError) => e.toString

    private def doSemanticCheck(currentHeader: BlockHeader, graph: MempoolGraph, meta: MempoolMetadata)(
      txToValidate: IoTransaction
    ): EitherT[F, String, IoTransaction] = {
      val semanticContextTemplate = StaticTransactionValidationContext(
        currentHeader.id,
        Seq.empty,
        currentHeader.height,
        currentHeader.slot
      )

      val semanticContext =
        if (meta.txCount < config.useMempoolForSemanticIfLess)
          semanticContextTemplate.copy(prefix = graph.transactions.values.toSeq)
        else semanticContextTemplate

      EitherT(
        semanticValidationAlgebra.validate(semanticContext)(txToValidate).map(_.toEither.leftMap(_.mkString_(",")))
      )
    }

    implicit val authError: Show[TransactionAuthorizationError] = (e: TransactionAuthorizationError) => e.toString

    private def doAuthCheck(
      currentHeader: BlockHeader
    )(txToValidate: IoTransaction): EitherT[F, String, IoTransaction] = {
      val authContext = QuivrContext.forProposedBlock(currentHeader.height, currentHeader.slot, txToValidate)
      EitherT(transactionAuthorizationVerifier.validate(authContext)(txToValidate).map(_.leftMap(e => show"$e")))
    }
  }

  case class MempoolMetadata(txCount: Long)

  private object MempoolMetadata {

    def apply(graph: MempoolGraph): MempoolMetadata =
      MempoolMetadata(graph.transactions.size)
  }
}
