package co.topl.consensus.algebras

trait ChainSelectionAlgebra[F[_], A] {

  /**
   * Compare values `x` and `y`.  If `x` is "better" than `y`, some value > 0 is returned.  If `x` and `y` are equal,
   * 0 is returned.  If `x` is "worse" than `y`, some value < 0 is returned.
   */
  def compare(x: A, y: A): F[Int]

  /**
   * Return smallest possible remote height which is enough to make decision which chain is better
   * @param currentHeight current chain height
   * @param commonHeight common ancestor height
   * @param proposedHeight other chain height
   * @return smallest possible height enough to make decision which chain is better
   */
  def enoughHeightToCompare(currentHeight: Long, commonHeight: Long, proposedHeight: Long): F[Long]
}
