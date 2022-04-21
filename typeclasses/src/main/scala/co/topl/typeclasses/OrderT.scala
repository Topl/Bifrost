package co.topl.typeclasses

trait OrderT[F[_], A] {

  /**
   * Compare values `x` and `y`.  If `x` is "better" than `y`, some value > 0 is returned.  If `x` and `y` are equal,
   * 0 is returned.  If `x` is "worse" than `y`, some value < 0 is returned.
   */
  def compare(x: A, y: A): F[Int]
}
