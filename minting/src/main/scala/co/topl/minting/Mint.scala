package co.topl.minting

import simulacrum.{op, typeclass}

import scala.concurrent.Future

@typeclass trait Mint[T] {
  @op("nextValue") def nextValueAfter(previousValue: T): Future[T]
}
