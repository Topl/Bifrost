package co.topl.networking.fsnetwork

import cats.data.NonEmptyChain
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.validation.TransactionSyntaxError
import co.topl.consensus.models.BlockId
import co.topl.models.TxRoot
import co.topl.typeclasses.implicits._
import co.topl.networking.fsnetwork.P2PShowInstances._

sealed abstract class BlockDownloadError extends Exception {
  def notCritical: Boolean
}

object BlockDownloadError {
  sealed trait BlockHeaderDownloadError extends BlockDownloadError

  object BlockHeaderDownloadError {

    case object HeaderNotFoundInPeer extends BlockHeaderDownloadError {
      override def toString: String = "Block body had been not found in peer"
      override val notCritical: Boolean = false
    }

    case class HeaderHaveIncorrectId(expected: BlockId, actual: BlockId) extends BlockHeaderDownloadError {
      override def toString: String = show"Peer returns header with bad id: expected $expected, actual $actual"
      override val notCritical: Boolean = false
    }

    case class UnknownError(ex: Throwable) extends BlockHeaderDownloadError {
      this.initCause(ex)

      override def toString: String =
        show"Unknown error during getting header from peer due next throwable ${ex.toString}"
      override val notCritical: Boolean = true
    }
  }

  sealed trait BlockBodyOrTransactionError extends BlockDownloadError

  object BlockBodyOrTransactionError {

    case object BodyNotFoundInPeer extends BlockBodyOrTransactionError {
      override def toString: String = "Block body had been not found in peer"
      override val notCritical: Boolean = false
    }

    case class BodyHaveIncorrectTxRoot(headerTxRoot: TxRoot, bodyTxRoot: TxRoot) extends BlockBodyOrTransactionError {
      override def toString: String = show"Peer returns body with bad txRoot: expected $headerTxRoot, got $bodyTxRoot"
      override val notCritical: Boolean = false
    }

    case class TransactionNotFoundInPeer(transactionId: TransactionId) extends BlockBodyOrTransactionError {
      override def toString: String = show"Peer have no transaction $transactionId despite having appropriate block"
      override val notCritical: Boolean = false
    }

    case class TransactionHaveIncorrectId(expected: TransactionId, actual: TransactionId)
        extends BlockBodyOrTransactionError {
      override def toString: String = show"Peer returns transaction with bad id: expected $expected, actual $actual"
      override val notCritical: Boolean = false
    }

    case class TransactionHaveIncorrectSyntax(
      transactionId: TransactionId,
      errors:        NonEmptyChain[TransactionSyntaxError]
    ) extends BlockBodyOrTransactionError {
      override def toString: String = show"Peer returns transaction $transactionId with incorrect syntax: $errors"

      override val notCritical: Boolean = false
    }

    case class UnknownError(ex: Throwable) extends BlockBodyOrTransactionError {
      this.initCause(ex)

      override def toString: String =
        show"Unknown error during getting block from peer due next throwable ${ex.toString}"

      override val notCritical: Boolean = true
    }
  }
}
