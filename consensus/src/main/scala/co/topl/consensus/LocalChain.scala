package co.topl.consensus

import cats.data.Validated
import cats.effect.Ref
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.models.SlotData
import co.topl.typeclasses.OrderT

object LocalChain {

  object Eval {

    def make[F[_]: Sync](
      initialHead:    SlotData,
      chainSelection: OrderT[F, SlotData]
    ): F[LocalChainAlgebra[F]] =
      Ref
        .of[F, SlotData](initialHead)
        .map(headRef =>
          new LocalChainAlgebra[F] {

            def isWorseThan(newHead: SlotData): F[Boolean] =
              head.flatMap(chainSelection.compare(_, newHead).map(_ < 0))

            def adopt(newHead: Validated.Valid[SlotData]): F[Unit] =
              headRef.update(_ => newHead.a)

            val head: F[SlotData] =
              headRef.get
          }
        )
  }
}
