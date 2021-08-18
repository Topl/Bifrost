package co.topl.minting

import simulacrum.typeclass

import scala.concurrent.Future

@typeclass trait Mint[T] {
  def nextValue(previousValue: T): Future[T]
}
