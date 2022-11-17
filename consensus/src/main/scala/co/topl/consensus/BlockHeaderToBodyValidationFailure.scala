package co.topl.consensus

import co.topl.models.TxRoot

sealed abstract class BlockHeaderToBodyValidationFailure

object BlockHeaderToBodyValidationFailure {
  case class IncorrectTxRoot(headerTxRoot: TxRoot, bodyTxRoot: TxRoot) extends BlockHeaderToBodyValidationFailure
}
