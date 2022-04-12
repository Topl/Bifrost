package co.topl.minting.algebras

import akka.NotUsed
import akka.stream.scaladsl.Source
import co.topl.models.BlockV2

/**
 * Perpetually mints new blocks
 * @tparam F[_] Base type constructor
 */
trait PerpetualBlockMintAlgebra[F[_]] {
  def blocks: F[Source[BlockV2, NotUsed]]
}
