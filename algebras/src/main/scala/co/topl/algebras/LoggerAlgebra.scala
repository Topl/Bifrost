package co.topl.algebras

trait LoggerAlgebra[F[_]] {
  def debug(message: String): F[Unit]
  def info(message:  String): F[Unit]
}

object LoggerAlgebra {
  def apply[F[_]: LoggerAlgebra]: LoggerAlgebra[F] = implicitly
}
