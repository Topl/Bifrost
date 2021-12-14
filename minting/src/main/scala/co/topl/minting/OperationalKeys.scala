package co.topl.minting

import cats._
import cats.data._
import cats.effect.Ref
import cats.effect.kernel.Concurrent
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.codecs.bytes.implicits._
import co.topl.crypto.keyfile.SecureStore
import co.topl.crypto.signing._
import co.topl.minting.algebras._
import co.topl.models._
import co.topl.typeclasses.KeyInitializer
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import scala.collection.immutable.LongMap

object OperationalKeys {

  object FromSecureStore {

    def make[F[_]: Concurrent: Logger](
      secureStore:                   SecureStore[F],
      clock:                         ClockAlgebra[F],
      vrfProof:                      VrfProofAlgebra[F],
      operationalPeriodLength:       Long,
      registrationOperationalPeriod: Long
    )(implicit
      kesProduct: KesProduct,
      ed25519:    Ed25519
    ): F[OperationalKeysAlgebra[F]] =
      for {
        initialSlot <- clock.globalSlot
        initialOperationalPeriod = initialSlot / operationalPeriodLength
        initialKeysOpt <- OptionT(
          consumeEvolvePersist(initialOperationalPeriod, registrationOperationalPeriod, secureStore)
        )
          .semiflatMap(prepareOperationalPeriodKeys(_, initialSlot, operationalPeriodLength, clock, vrfProof))
          .map(outs => LongMap.from(outs.map(o => o.slot -> o)))
          .value
        ref <- Ref.of((initialOperationalPeriod, initialKeysOpt))
      } yield make[F](
        secureStore,
        clock,
        vrfProof,
        operationalPeriodLength,
        registrationOperationalPeriod,
        ref
      )

    def make[F[_]: Monad: Logger](
      secureStore:                   SecureStore[F],
      clock:                         ClockAlgebra[F],
      vrfProof:                      VrfProofAlgebra[F],
      operationalPeriodLength:       Long,
      registrationOperationalPeriod: Long,
      ref:                           Ref[F, (Long, Option[LongMap[OperationalKeyOut]])]
    )(implicit
      kesProduct: KesProduct,
      ed25519:    Ed25519
    ): OperationalKeysAlgebra[F] = { (slot: Slot) =>
      val operationalPeriod = slot / operationalPeriodLength
      ref.get.flatMap {
        case (`operationalPeriod`, keysOpt) =>
          keysOpt.flatMap(_.get(slot)).pure[F]
        case _ =>
          OptionT(consumeEvolvePersist(operationalPeriod, registrationOperationalPeriod, secureStore))
            .semiflatMap(prepareOperationalPeriodKeys(_, slot, operationalPeriodLength, clock, vrfProof))
            .map(outs => LongMap.from(outs.map(o => o.slot -> o)))
            .semiflatTap(newKeys => ref.set(operationalPeriod -> newKeys.some))
            .flatTapNone(ref.set(operationalPeriod -> None))
            .subflatMap(_.get(slot))
            .value
      }
    }

    /**
     * Consume the key (and any previous/lingering keys) closest to the given `currentOperationalPeriod`.  If the closest
     * key is not yet at the `currentOperationalPeriod`, it is evolved up to the `currentOperationalPeriod`.  The
     * key for `currentOperationalPeriod + 1` is constructed and saved to disk.  The `currentOperationalPeriod` key is
     * returned.
     */
    private def consumeEvolvePersist[F[_]: Monad: Logger](
      currentOperationalPeriod:      Long,
      registrationOperationalPeriod: Long,
      secureStore:                   SecureStore[F]
    ): F[Option[SecretKeys.KesProduct]] = {
      for {
        (toErase, latest) <- OptionT(
          secureStore.list
            .map(_.flatMap(t => Chain.fromOption(t.toLongOption)).filter(_ <= currentOperationalPeriod).sorted.initLast)
        )
        _ <- OptionT.liftF(
          toErase
            .map(_.toString)
            .traverse(k => Logger[F].info(show"Erasing old key idx=$k").flatMap(_ => secureStore.erase(k)))
        )
        _       <- OptionT.liftF(Logger[F].info(show"Consuming key idx=$latest"))
        diskKey <- OptionT(secureStore.consume[SecretKeys.KesProduct](latest.toString))
        currentPeriodKey =
          if (latest === (currentOperationalPeriod - registrationOperationalPeriod)) diskKey
          else
            KesProduct.instance.update(
              diskKey,
              (currentOperationalPeriod - registrationOperationalPeriod).toInt
            )
        nextKeyIndex = currentOperationalPeriod - registrationOperationalPeriod + 1
        _ <- OptionT.liftF(Logger[F].info(show"Saving next key idx=$nextKeyIndex"))
        _ <- OptionT.liftF(
          secureStore.write(
            nextKeyIndex.toString,
            KesProduct.instance.update(
              currentPeriodKey,
              nextKeyIndex.toInt
            )
          )
        )
      } yield currentPeriodKey
    }.value

    /**
     * Using some KES child, construct the linear keys for the upcoming operational period.  A linear key is constructed
     * for each slot for which we _might_ be eligible for VRF.
     */
    private def prepareOperationalPeriodKeys[F[_]: Monad](
      kesChild:                SecretKeys.KesProduct,
      fromSlot:                Slot,
      operationalPeriodLength: Long,
      clock:                   ClockAlgebra[F],
      vrfProof:                VrfProofAlgebra[F]
    )(implicit
      kesProduct: KesProduct,
      ed25519:    Ed25519
    ): F[Vector[OperationalKeyOut]] =
      for {
        epoch           <- clock.epochOf(fromSlot)
        ineligibleSlots <- vrfProof.ineligibleSlots(epoch).map(_.toSet)
        slots = Vector
          .tabulate((operationalPeriodLength - (fromSlot % operationalPeriodLength)).toInt)(_ + fromSlot)
          .filterNot(ineligibleSlots)
        keys = prepareOperationalPeriodKeys(kesChild, slots)
      } yield keys

    /**
     * From some "child" KES key, create several SKs for each slot in the given list of slots
     */
    private def prepareOperationalPeriodKeys(kesChild: SecretKeys.KesProduct, slots: Vector[Slot])(implicit
      kesProduct:                                      KesProduct,
      ed25519:                                         Ed25519
    ): Vector[OperationalKeyOut] =
      slots.map { slot =>
        val sk = KeyInitializer[SecretKeys.Ed25519].random()
        val signedVk = kesProduct.sign(kesChild, ed25519.getVerificationKey(sk).bytes.data)
        OperationalKeyOut(slot, sk, signedVk)
      }
  }
}
