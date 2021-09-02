package co.topl.minting

trait Mint[F[_], T, Algebra] {
  def next(interpreter: Algebra): F[T]
}
