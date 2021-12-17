package co.topl.minting

import cats._
import cats.data._
import cats.effect.Ref
import cats.effect.kernel.Concurrent
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.codecs.bytes.implicits._
import co.topl.consensus.algebras.EtaCalculationAlgebra
import co.topl.crypto.keyfile.SecureStore
import co.topl.crypto.signing._
import co.topl.minting.algebras._
import co.topl.models._
import co.topl.typeclasses.KeyInitializer
import co.topl.typeclasses.implicits._
import com.google.common.primitives.Longs
import org.typelevel.log4cats.Logger

import java.util.UUID
import scala.collection.immutable.LongMap

object OperationalKeys {

  object FromSecureStore {

    def make[F[_]: Concurrent: Logger](
      secureStore:                 SecureStore[F],
      clock:                       ClockAlgebra[F],
      vrfProof:                    VrfProofAlgebra[F],
      etaCalculation:              EtaCalculationAlgebra[F],
      parentSlotId:                SlotId,
      operationalPeriodLength:     Long,
      activationOperationalPeriod: Long
    )(implicit
      kesProduct: KesProduct,
      ed25519:    Ed25519
    ): F[OperationalKeysAlgebra[F]] =
      for {
        initialSlot <- clock.globalSlot
        initialOperationalPeriod = initialSlot / operationalPeriodLength
        initialKeysOpt <- OptionT(
          consumeEvolvePersist(
            (initialOperationalPeriod - activationOperationalPeriod).toInt,
            secureStore,
            prepareOperationalPeriodKeys(
              _,
              initialSlot,
              parentSlotId,
              operationalPeriodLength,
              clock,
              vrfProof,
              etaCalculation
            )
          )
        ).value
        ref <- Ref.of((initialOperationalPeriod, initialKeysOpt))
      } yield make[F](
        secureStore,
        clock,
        vrfProof,
        etaCalculation,
        operationalPeriodLength,
        activationOperationalPeriod,
        ref
      )

//    def a(slot: Slot, operationalPeriod: Long, epoch: Epoch)
//    def slotWithinEpoch(slot:                           Slot, slotsPerEpoch:              Long) = slot % slotsPerEpoch
//    def slotWithinOperationalPeriod(slot:               Slot, slotsPerOperationalPeriod:  Long) = slot % slotsPerOperationalPeriod
//
//    def operationalPeriodWithinEpoch(operationalPeriod: Long, operationalPeriodsPerEpoch: Long) =
//      operationalPeriod % operationalPeriodsPerEpoch
    // This _should_ be less than 2^18 (or the maximum size of the KesProduct key)
    // def keyTimeStep(currentOperationalPeriod: Long, activationOperationalPeriod: Long) =
    //   currentOperationalPeriod - activationOperationalPeriod

    def make[F[_]: MonadError[*[_], Throwable]: Logger](
      secureStore:                 SecureStore[F],
      clock:                       ClockAlgebra[F],
      vrfProof:                    VrfProofAlgebra[F],
      etaCalculation:              EtaCalculationAlgebra[F],
      operationalPeriodLength:     Long,
      activationOperationalPeriod: Long,
      ref:                         Ref[F, (Long, Option[LongMap[OperationalKeyOut]])]
    )(implicit
      kesProduct: KesProduct,
      ed25519:    Ed25519
    ): OperationalKeysAlgebra[F] = { (slot: Slot, parentSlotId: SlotId) =>
      val operationalPeriod = slot / operationalPeriodLength
      ref.get.flatMap {
        case (`operationalPeriod`, keysOpt) =>
          keysOpt.flatMap(_.get(slot)).pure[F]
        case _ =>
          OptionT(
            consumeEvolvePersist(
              (operationalPeriod - activationOperationalPeriod).toInt,
              secureStore,
              use = prepareOperationalPeriodKeys(
                _,
                slot,
                parentSlotId,
                operationalPeriodLength,
                clock,
                vrfProof,
                etaCalculation
              )
            )
          )
            .semiflatTap(newKeys => ref.set(operationalPeriod -> newKeys.some))
            .flatTapNone(ref.set(operationalPeriod -> None))
            .subflatMap(_.get(slot))
            .value
      }
    }

    /**
     * Consume the key (and any previous/lingering keys) closest to the given `currentOperationalPeriod`.  If the closest
     * key is not yet at the `currentOperationalPeriod`, it is updated up to the `currentOperationalPeriod`.  The
     * key for `timeStep + 1` is constructed and saved to disk.
     */
    private def consumeEvolvePersist[F[_]: MonadError[*[_], Throwable]: Logger, T](
      timeStep:            Int,
      secureStore:         SecureStore[F],
      use:                 SecretKeys.KesProduct => F[T]
    )(implicit kesProduct: KesProduct): F[Option[T]] = {
      for {
        fileName <- OptionT.liftF(secureStore.list.flatMap {
          case Chain(fileName) => fileName.pure[F]
          case _ =>
            MonadError[F, Throwable].raiseError[String](
              new IllegalStateException("SecureStore contained 0 or multiple keys")
            )
        })
        _       <- OptionT.liftF(Logger[F].info(show"Consuming key id=$fileName"))
        diskKey <- OptionT(secureStore.consume[SecretKeys.KesProduct](fileName))
        latest = kesProduct.getCurrentStep(diskKey)
        currentPeriodKey =
          if (latest === timeStep) diskKey
          else kesProduct.update(diskKey, timeStep.toInt)
        res <- OptionT.liftF(use(currentPeriodKey))
        nextTimeStep = timeStep + 1
        _ <- OptionT.liftF(Logger[F].info(show"Saving next key idx=$nextTimeStep"))
        _ <- OptionT.liftF(
          secureStore.write(
            UUID.randomUUID().toString,
            kesProduct.update(
              currentPeriodKey,
              nextTimeStep.toInt
            )
          )
        )
      } yield res
    }.value

    /**
     * Using some KES child, construct the linear keys for the upcoming operational period.  A linear key is constructed
     * for each slot for which we _might_ be eligible for VRF.
     */
    private def prepareOperationalPeriodKeys[F[_]: Monad: Logger](
      kesChild:                SecretKeys.KesProduct,
      fromSlot:                Slot,
      parentSlotId:            SlotId,
      operationalPeriodLength: Long,
      clock:                   ClockAlgebra[F],
      vrfProof:                VrfProofAlgebra[F],
      etaCalculation:          EtaCalculationAlgebra[F]
    )(implicit
      kesProduct: KesProduct,
      ed25519:    Ed25519
    ): F[LongMap[OperationalKeyOut]] =
      for {
        epoch <- clock.epochOf(fromSlot)
        eta   <- etaCalculation.etaToBe(parentSlotId, fromSlot)
        // TODO: Bound this value to the slots within this operational period
        ineligibleSlots <- vrfProof.ineligibleSlots(epoch, eta).map(_.toSet)
        slots = Vector
          .tabulate((operationalPeriodLength - (fromSlot % operationalPeriodLength)).toInt)(_ + fromSlot)
          .filterNot(ineligibleSlots)
        _ <- Logger[F].info(s"Preparing linear keys.  count=${slots.size}")
        outs = prepareOperationalPeriodKeys(kesChild, slots)
        mappedKeys = LongMap.from(outs.map(o => o.slot -> o))
      } yield mappedKeys

    /**
     * From some "child" KES key, create several SKs for each slot in the given list of slots
     */
    private def prepareOperationalPeriodKeys(kesChild: SecretKeys.KesProduct, slots: Vector[Slot])(implicit
      kesProduct:                                      KesProduct,
      ed25519:                                         Ed25519
    ): Vector[OperationalKeyOut] =
      slots.map { slot =>
        val sk = KeyInitializer[SecretKeys.Ed25519].random()
        val signedVk =
          kesProduct.sign(kesChild, (ed25519.getVerificationKey(sk).bytes.data ++ Bytes(Longs.toByteArray(slot))))
        OperationalKeyOut(slot, sk, signedVk, kesProduct.getVerificationKey(kesChild))
      }
  }
}
