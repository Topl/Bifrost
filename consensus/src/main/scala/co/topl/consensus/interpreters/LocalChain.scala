package co.topl.consensus.interpreters

import cats.data.{EitherT, Validated}
import cats.effect.{Async, Ref, Resource}
import cats.effect.kernel.Sync
import cats.effect.implicits._
import cats.implicits._
import co.topl.consensus.algebras._
import co.topl.consensus.models._
import co.topl.eventtree.EventSourcedState
import co.topl.interpreters.BlockHeightTree
import co.topl.typeclasses.implicits._
import fs2.concurrent.Topic
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats._

object LocalChain {

  def make[F[_]: Async](
    genesis:                       SlotData,
    initialHead:                   SlotData,
    chainSelection:                ChainSelectionAlgebra[F],
    onAdopted:                     BlockId => F[Unit],
    blockHeightsEventSourcedState: EventSourcedState[F, BlockHeightTree.State[F], BlockId]
  ): Resource[F, LocalChainAlgebra[F]] = {
    val _g = genesis
    val _chainSelection = chainSelection
    (
      Resource.make(Topic[F, BlockId])(_.close.void),
      Ref.of[F, SlotData](initialHead).toResource
    ).mapN((adoptionsTopic, headRef) =>
      new LocalChainAlgebra[F] {
        implicit private val logger: SelfAwareStructuredLogger[F] =
          Slf4jLogger.getLoggerFromName[F]("Bifrost.LocalChain")

        override val chainSelection: F[ChainSelectionAlgebra[F]] = _chainSelection.pure[F]

        def adopt(slotData: SlotData): F[Unit] =
          Sync[F].uncancelable(_ =>
            onAdopted(slotData.slotId.blockId) >>
            headRef.set(slotData) >>
            EitherT(adoptionsTopic.publish1(slotData.slotId.blockId))
              .leftMap(_ => new IllegalStateException("LocalChain topic unexpectedly closed"))
              .rethrowT >>
            Logger[F].info(
              show"Adopted head block" +
              show" id=${slotData.slotId.blockId}" +
              show" height=${slotData.height}" +
              show" slot=${slotData.slotId.slot}"
            )
          )

        override def adoptions: F[fs2.Stream[F, BlockId]] =
          Async[F].delay(adoptionsTopic.subscribeUnbounded)

        val head: F[SlotData] =
          headRef.get

        val genesis: F[SlotData] =
          _g.pure[F]

        def blockIdAtHeight(height: Long): F[Option[BlockId]] =
          head.map(_.slotId.blockId).flatMap(blockHeightsEventSourcedState.useStateAt(_)(_.apply(height)))
      }
    )
  }
}
