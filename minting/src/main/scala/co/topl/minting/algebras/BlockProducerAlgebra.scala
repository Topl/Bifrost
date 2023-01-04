package co.topl.minting.algebras

import akka.NotUsed
import akka.stream.scaladsl.Source
import co.topl.models.Block

/**
 * Perpetually mints new blocks
 * @tparam F[_] Base type constructor
 */
trait BlockProducerAlgebra[F[_]] {
  def blocks: F[Source[Block, NotUsed]]
}
