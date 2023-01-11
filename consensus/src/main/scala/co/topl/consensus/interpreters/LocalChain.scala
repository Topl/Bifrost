package co.topl.consensus.interpreters

import cats.data.Validated
import cats.effect.Ref
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.consensus.algebras.{ChainSelectionAlgebra, LocalChainAlgebra}
import co.topl.models.{SlotData, TypedIdentifier}

object LocalChain {

  def make[F[_]: Sync](
    initialHead:    SlotData,
    chainSelection: ChainSelectionAlgebra[F, SlotData],
    onAdopted:      TypedIdentifier => F[Unit]
  ): F[LocalChainAlgebra[F]] =
    Ref
      .of[F, SlotData](initialHead)
      .map(headRef =>
        new LocalChainAlgebra[F] {

          def isWorseThan(newHead: SlotData): F[Boolean] =
            head.flatMap(chainSelection.compare(_, newHead).map(_ < 0))

          def adopt(newHead: Validated.Valid[SlotData]): F[Unit] =
            Sync[F].uncancelable(_ => onAdopted(newHead.a.slotId.blockId) >> headRef.update(_ => newHead.a))

          val head: F[SlotData] =
            headRef.get
        }
      )
}
