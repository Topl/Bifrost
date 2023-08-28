package co.topl.consensus.interpreters

import cats.data.Validated
import cats.effect.Ref
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.consensus.algebras.ChainSelectionAlgebra
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.SlotData
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.SelfAwareStructuredLogger

object LocalChain {

  def make[F[_]: Sync](
    genesis:        SlotData,
    initialHead:    SlotData,
    chainSelection: ChainSelectionAlgebra[F, SlotData],
    onAdopted:      BlockId => F[Unit]
  ): F[LocalChainAlgebra[F]] = {
    val _g = genesis
    Ref
      .of[F, SlotData](initialHead)
      .map(headRef =>
        new LocalChainAlgebra[F] {
          implicit private val logger: SelfAwareStructuredLogger[F] =
            Slf4jLogger.getLoggerFromName[F]("Bifrost.LocalChain")

          def couldBeWorse(newHead: SlotData): F[Boolean] =
            head.map(slotData => slotData.height <= newHead.height && slotData.slotId.blockId != newHead.slotId.blockId)

          def isWorseThan(newHead: SlotData): F[Boolean] =
            head.flatMap(chainSelection.compare(_, newHead).map(_ < 0))

          def adopt(newHead: Validated.Valid[SlotData]): F[Unit] = {
            val slotData = newHead.a
            Sync[F].uncancelable(_ =>
              onAdopted(slotData.slotId.blockId) >>
              headRef.set(slotData) >>
              Logger[F].info(
                show"Adopted head block" +
                show" id=${slotData.slotId.blockId}" +
                show" height=${slotData.height}" +
                show" slot=${slotData.slotId.slot}"
              )
            )
          }

          val head: F[SlotData] =
            headRef.get

          val genesis: F[SlotData] =
            _g.pure[F]
        }
      )
  }
}
