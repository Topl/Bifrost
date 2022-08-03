package co.topl.genus.typeclasses

import cats.implicits._
import cats.Monad
import co.topl.genus.algebras.ChainHeight
import co.topl.genus.types.BlockHeight
import simulacrum.typeclass

@typeclass
trait WithMaxBlockHeight[T] {
  def withMaxBlockHeight(value: T, height: BlockHeight): T

  final def withConfirmationDepth[F[_]: Monad: ChainHeight](value: T, confirmationDepth: Int): F[T] =
    for {
      currentHeight <- ChainHeight[F].get
      valueWithMaxBlockHeight = this.withMaxBlockHeight(value, BlockHeight(currentHeight.value - confirmationDepth))
    } yield valueWithMaxBlockHeight
}
