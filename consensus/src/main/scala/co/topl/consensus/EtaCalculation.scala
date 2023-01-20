package co.topl.consensus

import cats.MonadError
import cats.data.NonEmptyChain
import cats.effect.{Clock, Sync}
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{ClockAlgebra, UnsafeResource}
import co.topl.consensus.algebras.EtaCalculationAlgebra
import co.topl.crypto.hash.{Blake2b256, Blake2b512}
import co.topl.crypto.signing.Ed25519VRF
import co.topl.models._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import scalacache.caffeine.CaffeineCache

object EtaCalculation {

  object Eval {

    def make[F[_]: Clock: Sync: Logger](
      slotDataCache:      SlotDataCache[F],
      clock:              ClockAlgebra[F],
      genesisEta:         Eta,
      blake2b256Resource: UnsafeResource[F, Blake2b256],
      blake2b512Resource: UnsafeResource[F, Blake2b512]
    ): F[EtaCalculationAlgebra[F]] =
      for {
        implicit0(cache: CaffeineCache[F, Bytes, Eta]) <- CaffeineCache[F, Bytes, Eta]
        slotsPerEpoch                                  <- clock.slotsPerEpoch
        impl = new Impl[F](slotDataCache, clock, genesisEta, slotsPerEpoch, blake2b256Resource, blake2b512Resource)
      } yield impl

    private class Impl[F[_]: Clock: Sync: Logger](
      slotDataCache:      SlotDataCache[F],
      clock:              ClockAlgebra[F],
      genesisEta:         Eta,
      slotsPerEpoch:      Long,
      blake2b256Resource: UnsafeResource[F, Blake2b256],
      blake2b512Resource: UnsafeResource[F, Blake2b512]
    )(implicit cache: CaffeineCache[F, Bytes, Eta])
        extends EtaCalculationAlgebra[F] {

      private val twoThirdsLength = slotsPerEpoch * 2 / 3

      override def etaToBe(parentSlotId: SlotId, childSlot: Slot): F[Eta] =
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
              // TODO: If childEpoch - parentEpoch > 1, destroy the node
              // OR: childSlot - parentSlot > slotsPerEpoch
              case (_, _, parentSlotData) =>
                cache.cachingF(parentSlotId.blockId.allBytes)(ttl = None)(
                  locateTwoThirdsBest(parentSlotData)
                    .flatMap(calculate)
                    .flatTap(nextEta =>
                      Logger[F]
                        .info(show"Caching child epoch's eta for parent id=${parentSlotId.blockId} nextEta=$nextEta")
                    )
                )
            }

      /**
       * Given some header near the end of an epoch, traverse the chain (toward genesis) until reaching a block
       * that is inside of the 2/3 window of the epoch
       */
      private def locateTwoThirdsBest(from: SlotData): F[SlotData] =
        if (isWithinTwoThirds(from)) from.pure[F]
        else
          from
            .iterateUntilM(data => slotDataCache.get(data.parentSlotId.blockId))(isWithinTwoThirds)
            .flatTap(twoThirdsBest =>
              Logger[F].info(show"Located twoThirdsBest=${twoThirdsBest.slotId.blockId} from=${from.slotId.blockId}")
            )

      private def isWithinTwoThirds(from: SlotData): Boolean =
        from.slotId.slot % slotsPerEpoch <= twoThirdsLength

      /**
       * Compute the Eta value for the epoch containing the given header
       * @param twoThirdsBest The latest block header in some tine, but within the first 2/3 of the epoch
       */
      private def calculate(twoThirdsBest: SlotData): F[Eta] =
        cache.cachingF(twoThirdsBest.slotId.blockId.allBytes)(ttl = None)(
          for {
            epoch      <- clock.epochOf(twoThirdsBest.slotId.slot)
            epochRange <- clock.epochRange(epoch)
            epochData <- NonEmptyChain(twoThirdsBest).iterateUntilM(items =>
              slotDataCache.get(items.head.parentSlotId.blockId).map(items.prepend)
            )(items => items.head.parentSlotId.slot < epochRange.start)
            rhoValues = epochData.map(_.rho)
            nextEta <- calculate(previousEta = twoThirdsBest.eta, epoch + 1, rhoValues)
          } yield nextEta
        )

      /**
       * Calculate a new Eta value once all the necessary pre-requisites have been gathered
       */
      private def calculate(
        previousEta: Eta,
        epoch:       Epoch,
        rhoValues:   NonEmptyChain[Rho]
      ): F[Eta] =
        (Logger[F].info(
          show"Calculating new eta.  previousEta=$previousEta epoch=$epoch rhoValues=[${rhoValues.length}]{${rhoValues.head}..${rhoValues.last}}"
        ) >>
          blake2b512Resource
            .use(implicit blake2b512 => rhoValues.map(Ed25519VRF.rhoToRhoNonceHash).pure[F])
            .flatMap(calculateFromNonceHashValues(previousEta, epoch, _)))
          .flatTap(nextEta =>
            Logger[F].info(
              show"Finished calculating new eta.  previousEta=$previousEta epoch=$epoch rhoValues=[${rhoValues.length}]{${rhoValues.head}..${rhoValues.last}} nextEta=$nextEta"
            )
          )

      /**
       * Calculate a new Eta value once all the necessary pre-requisites have been gathered
       */
      private def calculateFromNonceHashValues(
        previousEta:        Eta,
        epoch:              Epoch,
        rhoNonceHashValues: NonEmptyChain[RhoNonceHash]
      ): F[Eta] =
        blake2b256Resource.use(
          _.hash(
            Bytes
              .concat(EtaCalculationArgs(previousEta, epoch, rhoNonceHashValues.toIterable).digestMessages)
          ).pure[F]
        )
    }

  }
}

private case class EtaCalculationArgs(previousEta: Eta, epoch: Epoch, rhoNonceHashValues: Iterable[RhoNonceHash]) {

  def digestMessages: List[Bytes] =
    List(previousEta.data) ++
    List(Bytes(BigInt(epoch).toByteArray)) ++
    rhoNonceHashValues.map(_.sizedBytes.data)
}
