package co.topl.networking.fsnetwork

import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.consensus.models.BlockId
import co.topl.models.TxRoot
import co.topl.typeclasses.implicits._

sealed abstract class BlockDownloadError extends Exception

object BlockDownloadError {
  sealed trait BlockHeaderDownloadError extends BlockDownloadError

  object BlockHeaderDownloadError {

    case object HeaderNotFoundInPeer extends BlockHeaderDownloadError {
      override def toString: String = "Block body has not found in peer"
    }

    case class HeaderHaveIncorrectId(expected: BlockId, actual: BlockId) extends BlockHeaderDownloadError {
      override def toString: String = show"Peer returns header with bad id: expected $expected, actual $actual"
    }

    case class UnknownError(ex: Throwable) extends BlockHeaderDownloadError {
      this.initCause(ex)

      override def toString: String = {
        val name = Option(ex.getClass.getName).getOrElse("")
        val message = Option(ex.getLocalizedMessage).getOrElse("")
        show"Unknown error during getting header from peer due next throwable $name : $message"
      }
    }
  }

  sealed trait BlockBodyDownloadError extends BlockDownloadError

  object BlockBodyDownloadError {

    case object BodyNotFoundInPeer extends BlockBodyDownloadError {
      override def toString: String = "Block body has not found in peer"
    }

    case class BodyHaveIncorrectTxRoot(headerTxRoot: TxRoot, bodyTxRoot: TxRoot) extends BlockBodyDownloadError {
      override def toString: String = show"Peer returns body with bad txRoot: expected $headerTxRoot, got $bodyTxRoot"
    }

    case class TransactionNotFoundInPeer(transactionId: TransactionId) extends BlockBodyDownloadError {
      override def toString: String = show"Peer have no transaction $transactionId despite having appropriate block"
    }

    case class TransactionHaveIncorrectId(expected: TransactionId, actual: TransactionId)
        extends BlockBodyDownloadError {
      override def toString: String = show"Peer returns transaction with bad id: expected $expected, actual $actual"
    }

    case class UnknownError(ex: Throwable) extends BlockBodyDownloadError {
      this.initCause(ex)

      override def toString: String = {
        val name = Option(ex.getClass.getName).getOrElse("")
        val message = Option(ex.getLocalizedMessage).getOrElse("")
        show"Unknown error during getting block from peer due next throwable $name : $message"
      }
    }
  }
}
