package co.topl.minting.algebras

import akka.stream.scaladsl.Source
import co.topl.models.BlockV2

/**
 * Perpetually mints new blocks
 * @tparam F[_] Base type constructor
 * @tparam G[_] Stream-like type constructor
 */
trait PerpetualBlockMintAlgebra[F[_], G[_]] {
  def blocks: F[G[BlockV2]]
}
