package co.topl.networking.fsnetwork

import cats.implicits._
import cats.data.NonEmptyChain
import co.topl.consensus.models.{BlockHeaderValidationFailure, BlockId}
import co.topl.ledger.models.BodyValidationError
import co.topl.models.p2p._
import org.apache.commons.lang3.exception.ExceptionUtils

sealed abstract class BlockApplyError extends Exception

object BlockApplyError {
  sealed abstract class HeaderApplyException extends BlockApplyError

  object HeaderApplyException {

    case class HeaderValidationException(blockId: BlockId, source: HostId, error: BlockHeaderValidationFailure)
        extends HeaderApplyException

    case class UnknownError(ex: Throwable) extends HeaderApplyException {
      this.initCause(ex)

      override def toString: String =
        show"Error applying block header due next throwable ${ex.toString} ${ExceptionUtils.getStackTrace(ex)}"
    }
  }

  sealed abstract class BodyApplyException extends BlockApplyError

  object BodyApplyException {

    case class BodyValidationException(blockId: BlockId, source: HostId, errors: NonEmptyChain[BodyValidationError])
        extends BodyApplyException

    case class UnknownError(ex: Throwable) extends BodyApplyException {
      this.initCause(ex)

      override def toString: String =
        show"Error applying block body due next throwable ${ex.toString} ${ExceptionUtils.getStackTrace(ex)}"
    }
  }
}
