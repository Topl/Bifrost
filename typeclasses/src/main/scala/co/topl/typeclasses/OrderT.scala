package co.topl.typeclasses

trait OrderT[F[_], A] {
  def compare(x: A, y: A): F[Int]
}
