package co.topl.genus.interpreters

import cats.effect.IO
import co.topl.genus.algebras.ChainHeight
import co.topl.genus.types.BlockHeight

object MockChainHeight {

  def withHeight(height: BlockHeight): ChainHeight[IO] =
    new ChainHeight[IO] {
      override def get: IO[BlockHeight] = IO(height)
    }
}
