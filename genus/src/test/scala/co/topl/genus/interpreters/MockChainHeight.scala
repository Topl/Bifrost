package co.topl.genus.interpreters

import cats.Applicative
import cats.implicits._
import co.topl.genus.algebras.ChainHeight
import co.topl.genus.types.BlockHeight

object MockChainHeight {

  def default[F[_]: Applicative]: ChainHeight[F] = withHeight(BlockHeight(100))

  def withHeight[F[_]: Applicative](height: BlockHeight): ChainHeight[F] =
    new ChainHeight[F] {
      override def get: F[BlockHeight] = height.pure[F]
    }
}
