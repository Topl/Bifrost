package co.topl.consensus.interpreters

import cats.effect.kernel.Sync
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxEq}
import co.topl.consensus.BlockHeaderToBodyValidationFailure
import co.topl.consensus.BlockHeaderToBodyValidationFailure.IncorrectTxRoot
import co.topl.consensus.algebras.BlockHeaderToBodyValidationAlgebra
import co.topl.models.Block
import co.topl.models.utility.Sized
import co.topl.models.utility.HasLength.instances._
import co.topl.typeclasses.implicits._
import scodec.bits.ByteVector

object BlockHeaderToBodyValidation {

  object Eval {

    def make[F[_]: Sync](): F[BlockHeaderToBodyValidationAlgebra[F]] = Sync[F].delay(new Impl[F]())

    private class Impl[F[_]: Sync]() extends BlockHeaderToBodyValidationAlgebra[F] {

      override def validate(block: Block): F[Either[BlockHeaderToBodyValidationFailure, Block]] =
        blockTxRootConsistent(block).pure[F]
    }

    private def blockTxRootConsistent(block: Block): Either[BlockHeaderToBodyValidationFailure, Block] = {
      val bodyMerkleTxRoot = block.body.merkleTreeRootHash
      val headerMerkleTxRoot = block.header.txRoot

      if (bodyMerkleTxRoot === Sized.strictUnsafe(ByteVector(headerMerkleTxRoot.toByteArray))) {
        Right(block)
      } else {
        Left(IncorrectTxRoot(Sized.strictUnsafe(ByteVector(headerMerkleTxRoot.toByteArray)), bodyMerkleTxRoot))
      }
    }

  }
}
