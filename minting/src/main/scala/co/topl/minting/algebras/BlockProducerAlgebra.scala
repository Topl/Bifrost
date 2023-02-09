package co.topl.minting.algebras

import co.topl.models.Block
import fs2.Stream

/**
 * Perpetually mints new blocks
 * @tparam F[_] Base type constructor
 */
trait BlockProducerAlgebra[F[_]] {
  def blocks: F[Stream[F, Block]]
}
