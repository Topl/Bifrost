package co.topl.genusLibrary.algebras

/**
 * Updater of the chain in the data store.
 * @tparam F the effect-ful context to retrieve the value in
 */
trait ChainUpdaterAlgebra[F[_]] {

  /**
   * Updates the chain in the data store.
   * @return Unit
   */
  def update: F[Unit]

}
