package co.topl.networking.fsnetwork

import cats.implicits._
import cats.data.NonEmptyChain
import co.topl.consensus.models.{BlockHeaderValidationFailure, BlockId}
import co.topl.ledger.models.BodyValidationError

sealed abstract class BlockApplyError extends Exception

object BlockApplyError {
  sealed abstract class HeaderApplyException extends BlockApplyError

  object HeaderApplyException {

    case class HeaderValidationException(blockId: BlockId, source: HostId, error: BlockHeaderValidationFailure)
        extends HeaderApplyException

    case class UnknownError(ex: Throwable) extends HeaderApplyException {
      this.initCause(ex)

      override def toString: String = {
        val name = Option(ex.getClass.getName).getOrElse("")
        val message = Option(ex.getLocalizedMessage).getOrElse("")
        show"Unknown error during applying block header due next throwable $name : $message"
      }
    }
  }

  sealed abstract class BodyApplyException extends BlockApplyError

  object BodyApplyException {

    case class BodyValidationException(blockId: BlockId, source: HostId, errors: NonEmptyChain[BodyValidationError])
        extends BodyApplyException

    case class UnknownError(ex: Throwable) extends BodyApplyException {
      this.initCause(ex)

      override def toString: String = {
        val name = Option(ex.getClass.getName).getOrElse("")
        val message = Option(ex.getLocalizedMessage).getOrElse("")
        show"Unknown error during applying block body due next throwable $name : $message"
      }
    }
  }
}
