package co.topl.consensus.interpreters

import cats.data.{EitherT, Validated}
import cats.effect.{Async, Ref, Resource}
import cats.effect.kernel.Sync
import cats.effect.implicits._
import cats.implicits._
import co.topl.consensus.algebras._
import co.topl.consensus.models._
import co.topl.typeclasses.implicits._
import fs2.concurrent.Topic
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats._

object LocalChain {

  def make[F[_]: Async](
    genesis:        SlotData,
    initialHead:    SlotData,
    chainSelection: ChainSelectionAlgebra[F, SlotData],
    onAdopted:      BlockId => F[Unit]
  ): Resource[F, LocalChainAlgebra[F]] = {
    val _g = genesis
    (
      Resource.make(Topic[F, BlockId])(_.close.void),
      Ref.of[F, SlotData](initialHead).toResource
    ).mapN((adoptionsTopic, headRef) =>
      new LocalChainAlgebra[F] {

        implicit private val logger: SelfAwareStructuredLogger[F] =
          Slf4jLogger.getLoggerFromName[F]("Bifrost.LocalChain")

        override val chainSelectionAlgebra: F[ChainSelectionAlgebra[F, SlotData]] = chainSelection.pure[F]

        def isWorseThan(newHead: SlotData): F[Boolean] =
          head.flatMap(chainSelection.compare(_, newHead).map(_ < 0))

        def adopt(newHead: Validated.Valid[SlotData]): F[Unit] = {
          val slotData = newHead.a
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
        }

        override def adoptions: F[fs2.Stream[F, BlockId]] =
          Async[F].delay(adoptionsTopic.subscribeUnbounded)

        val head: F[SlotData] =
          headRef.get

        val genesis: F[SlotData] =
          _g.pure[F]
      }
    )
  }
}
