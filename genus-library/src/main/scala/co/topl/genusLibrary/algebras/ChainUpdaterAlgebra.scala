package co.topl.genusLibrary.algebras

/**
 * Updater of the chain in the data store.
 *
 * Single-responsibility principle for this algebra and the rest of algebras is enforced,
 * making each trait having a single responsibility.
 * It gives the developer much more flexibility on how to implement the traits.
 * For Topl's particular use case, one class/object should be created from each trait.
 * Concrete methods will be rather large and may be hard to read on a single file.
 *
 * @tparam F the effect-ful context to retrieve the value in
 */
trait ChainUpdaterAlgebra[F[_]] {

  /**
   * Updates the chain in the data store.
   * @return Unit
   */
  def update: F[Unit]

}
