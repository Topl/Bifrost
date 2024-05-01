package co.topl.consensus.interpreters

import cats.data.{EitherT, Validated}
import cats.effect.{Async, Ref, Resource}
import cats.effect.kernel.Sync
import cats.effect.implicits._
import cats.implicits._
import co.topl.consensus.algebras._
import co.topl.consensus.models._
import co.topl.eventtree.EventSourcedState
import co.topl.typeclasses.implicits._
import fs2.concurrent.Topic
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats._
import co.topl.algebras.Stats
import io.circe.syntax._

object LocalChain {

  def make[F[_]: Async: Stats](
    genesis:         SlotData,
    initialHead:     SlotData,
    chainSelection:  ChainSelectionAlgebra[F, SlotData],
    onAdopted:       BlockId => F[Unit],
    blockHeightsESS: EventSourcedState[F, Long => F[Option[BlockId]], BlockId]
  ): Resource[F, LocalChainAlgebra[F]] = {
    val _g = genesis
    (
      Resource.make(Topic[F, BlockId])(_.close.void),
      Ref.of[F, SlotData](initialHead).toResource
    ).mapN((adoptionsTopic, headRef) =>
      new LocalChainAlgebra[F] {

        implicit private val logger: SelfAwareStructuredLogger[F] =
          Slf4jLogger.getLoggerFromName[F]("Bifrost.LocalChain")

        def isWorseThan(newHead: SlotData): F[Boolean] =
          head.flatMap(chainSelection.compare(_, newHead).map(_ < 0))

        // TODO add semaphore to avoid possible concurrency issue with adoption in the same time local / network
        def adopt(newHead: Validated.Valid[SlotData]): F[Unit] = {
          val slotData = newHead.a
          Sync[F].uncancelable(_ =>
            onAdopted(slotData.slotId.blockId) >>
            headRef.set(slotData) >>
            Stats[F].recordGauge(
              "bifrost_block_adoptions",
              "Block adoptions",
              Map("block_id" -> (show"${slotData.slotId.blockId}").asJson),
              slotData.height.asJson
            ) >>
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
        }

        override def adoptions: F[fs2.Stream[F, BlockId]] =
          Async[F].delay(adoptionsTopic.subscribeUnbounded)

        val head: F[SlotData] =
          headRef.get

        val genesis: F[SlotData] =
          _g.pure[F]

        def blockIdAtHeight(height: Long): F[Option[BlockId]] =
          if (height == _g.height)
            _g.slotId.blockId.some.pure[F]
          else if (height > _g.height)
            head.map(_.slotId.blockId).flatMap(blockHeightsESS.useStateAt(_)(_.apply(height)))
          else if (height == 0L)
            head.map(_.slotId.blockId.some)
          else
            head
              .map(_.height + height)
              .flatMap(targetHeight =>
                if (targetHeight < _g.height) none[BlockId].pure[F]
                else blockIdAtHeight(targetHeight)
              )
      }
    )
  }
}
