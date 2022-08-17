package co.topl.genus.algebras

import co.topl.genus.types.BlockHeight

/**
 * Represents the current height of the largest chain in the data store.
 * @tparam F the effect-ful context to retrieve the value in
 */
trait ChainHeight[F[_]] {

  /**
   * Retrieves the current height of the largest chain as an instance of [[BlockHeight]].
   * @return the current height
   */
  def get: F[BlockHeight]
}

object ChainHeight {
  def apply[F[_]](implicit chainHeight: ChainHeight[F]): ChainHeight[F] = chainHeight
}
