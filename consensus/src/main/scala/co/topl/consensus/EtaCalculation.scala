package co.topl.consensus

import cats.MonadError
import cats.data.{EitherT, NonEmptyChain}
import cats.effect.{Clock, Sync}
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.consensus.algebras.EtaCalculationAlgebra
import co.topl.crypto.hash.blake2b256
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized
import co.topl.typeclasses.BlockGenesis
import co.topl.typeclasses.implicits._
import scalacache._
import scalacache.caffeine.CaffeineCache

import scala.concurrent.duration._

object EtaCalculation {

  object Eval {

    implicit private val cacheConfig: CacheConfig = CacheConfig(cacheKeyBuilder[TypedIdentifier])

    def make[F[_]: Clock: Sync: MonadError[*[_], Throwable]](
      slotDataCache: SlotDataCache[F],
      clock:         ClockAlgebra[F],
      genesisEta:    Eta
    ): F[EtaCalculationAlgebra[F]] =
      CaffeineCache[F, Eta].map(implicit cache =>
        new EtaCalculationAlgebra[F] {

          override def etaToBe(parentSlotId: SlotId, childSlot: Slot): F[Eta] =
            clock.slotsPerEpoch.flatMap(slotsPerEpoch =>
              if (childSlot < slotsPerEpoch) genesisEta.pure[F]
              else
                (
                  clock.epochOf(parentSlotId.slot),
                  clock.epochOf(childSlot),
                  slotDataCache.get(parentSlotId.blockId)
                ).tupled
                  .flatMap {
                    case (parentEpoch, childEpoch, parentSlotData) if parentEpoch === childEpoch =>
                      parentSlotData.eta.pure[F]
                    case (_, _, parentSlotData) =>
                      locateTwoThirdsBest(parentSlotData).flatMap(calculate)
                  }
            )

          /**
           * Given some header near the end of an epoch, traverse the chain (toward genesis) until reaching a block
           * that is inside of the 2/3 window of the epoch
           */
          private def locateTwoThirdsBest(child: SlotData): F[SlotData] =
            for {
              epochLength <- clock.slotsPerEpoch
              twoThirdsLength = epochLength * 2 / 3
              twoThirdsBest <- child.iterateUntilM(data => slotDataCache.get(data.parentSlotId.blockId))(data =>
                data.slotId.slot % epochLength < twoThirdsLength
              )
            } yield twoThirdsBest

          /**
           * Compute the Eta value for the epoch containing the given header
           * @param twoThirdsBest The latest block header in some tine, but within the first 2/3 of the epoch
           */
          private def calculate(twoThirdsBest: SlotData): F[Eta] =
            cachingF(twoThirdsBest.slotId.blockId)(ttl = Some(1.day))(
              for {
                epoch      <- clock.epochOf(twoThirdsBest.slotId.slot)
                epochRange <- clock.epochRange(epoch)
                epochData <- collectUntil(twoThirdsBest)(head =>
                  head.parentSlotId.slot < epochRange.start || head.parentSlotId.slot === BlockGenesis.ParentSlot
                )
                previousEta = epochData.head.eta
                nextEta = calculate(previousEta, epoch, epochData.map(_.rho))
              } yield nextEta
            )

          /**
           * Traverse a tine backwards from some "head" block until a predicate is met
           * @param head The starting point for the backwards-traversal
           * @param predicate A condition (applied to the traversal's current earliest block) which stops the traversal
           */
          private def collectUntil(head: SlotData)(predicate: SlotData => Boolean): F[NonEmptyChain[SlotData]] =
            EitherT(
              NonEmptyChain(head)
                .asLeft[NonEmptyChain[SlotData]]
                .iterateUntilM {
                  // Treat "Left" as incomplete
                  case Left(acc) =>
                    if (predicate(acc.head))
                      acc.asRight[NonEmptyChain[SlotData]].pure[F]
                    else
                      slotDataCache
                        .get(acc.head.parentSlotId.blockId)
                        .map(acc.prepend(_).asLeft[NonEmptyChain[SlotData]])
                  case r =>
                    r.pure[F]
                }(_.isRight)
            ).merge

          /**
           * Calculate a new Eta value once all the nececssary pre-requisites have been gathered
           */
          private def calculate(previousEta: Eta, epoch: Epoch, rhoValues: NonEmptyChain[Rho]): Eta =
            Sized
              .strictUnsafe(
                Bytes(
                  blake2b256
                    .hash(
                      None,
                      EtaCalculationArgs(previousEta, epoch, rhoValues.toIterable).digestMessages: _*
                    )
                    .value
                )
              )
        }
      )
  }
}

private case class EtaCalculationArgs(previousEta: Eta, epoch: Epoch, rhoValues: Iterable[Rho]) {

  def digestMessages: List[Array[Byte]] =
    (List(previousEta.data) ++ List(Bytes(BigInt(epoch).toByteArray)) ++ rhoValues
      .map(_.data))
      .map(_.toArray)
}
