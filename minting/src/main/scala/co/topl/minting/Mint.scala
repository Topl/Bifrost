package co.topl.minting

trait Mint[F[_], T] {
  def nextValueAfter(previousValue: T): F[T]
}

object Mint {

  trait Ops[F[_], T] {
    def typeClassInstance: Mint[F, T]
    def self: T
    def nextValue: F[T] = typeClassInstance.nextValueAfter(self)
  }

  object ops {

    implicit def toOps[F[_], T](target: T)(implicit m: Mint[F, T]): Ops[F, T] =
      new Ops[F, T] {
        override def typeClassInstance: Mint[F, T] = m
        override def self: T = target
      }
  }
}
