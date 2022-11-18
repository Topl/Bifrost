package co.topl.consensus.interpreters

import cats.effect.kernel.Sync
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxEq}
import co.topl.consensus.BlockHeaderToBodyValidationFailure
import co.topl.consensus.BlockHeaderToBodyValidationFailure.IncorrectTxRoot
import co.topl.consensus.algebras.BlockHeaderToBodyValidationAlgebra
import co.topl.models.BlockV2
import co.topl.typeclasses.implicits._

object BlockHeaderToBodyValidation {

  object Eval {

    def make[F[_]: Sync](): F[BlockHeaderToBodyValidationAlgebra[F]] = Sync[F].delay(new Impl[F]())

    private class Impl[F[_]: Sync]() extends BlockHeaderToBodyValidationAlgebra[F] {

      override def validate(block: BlockV2): F[Either[BlockHeaderToBodyValidationFailure, BlockV2]] =
        blockTxRootConsistent(block).pure[F]
    }

    private def blockTxRootConsistent(block: BlockV2): Either[BlockHeaderToBodyValidationFailure, BlockV2] = {
      val bodyMerkleTxRoot = block.blockBodyV2.merkleTreeRootHash
      val headerMerkleTxRoot = block.headerV2.txRoot

      if (bodyMerkleTxRoot === headerMerkleTxRoot) {
        Right(block)
      } else {
        Left(IncorrectTxRoot(headerMerkleTxRoot, bodyMerkleTxRoot))
      }
    }

  }
}
