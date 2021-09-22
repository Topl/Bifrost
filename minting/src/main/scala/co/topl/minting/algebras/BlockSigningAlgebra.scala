package co.topl.minting.algebras

import co.topl.models.BlockV2

/**
 * Signs unsigned blocks.  Interpreters may need to encapsulate a private key or a KeyEvolver.
 */
trait BlockSigningAlgebra[F[_]] {
  def sign(unsignedBlock: BlockV2.Unsigned): F[BlockV2]
}
