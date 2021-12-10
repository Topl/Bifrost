package co.topl.minting

import cats._
import cats.data._
import cats.effect.Ref
import cats.effect.kernel.Concurrent
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.codecs.bytes.implicits._
import co.topl.crypto.keyfile.SecureStore
import co.topl.crypto.signing._
import co.topl.minting.algebras._
import co.topl.models._
import co.topl.typeclasses.KeyInitializer
import co.topl.typeclasses.implicits._

import scala.collection.immutable.LongMap
import ClockAlgebra.implicits._

object KeyEvolver {

  object FromSecureStore {

    def make[F[_]: Concurrent](
      secureStore:                   SecureStore[F],
      clock:                         ClockAlgebra[F],
      vrfProof:                      VrfProofAlgebra[F],
      operationalPeriodLength:       Long,
      registrationOperationalPeriod: Long
    )(implicit
      kesProduct: KesProduct,
      ed25519:    Ed25519
    ): F[KeyEvolverAlgebra[F]] =
      for {
        initialOperationalPeriod <- clock.globalSlot.map(_ / operationalPeriodLength)
        initialKeysOpt <- consumeEvolvePersist(initialOperationalPeriod, registrationOperationalPeriod, secureStore)
          // TODO: Transform the KES child key into the linear keys for the period
          .map(_ => None)
        ref <- Ref.of((initialOperationalPeriod, initialKeysOpt))
      } yield make[F](
        secureStore,
        clock,
        vrfProof,
        operationalPeriodLength,
        registrationOperationalPeriod,
        initialOperationalPeriod,
        initialKeysOpt,
        ref
      )

    def make[F[_]: Monad](
      secureStore:                   SecureStore[F],
      clock:                         ClockAlgebra[F],
      vrfProof:                      VrfProofAlgebra[F],
      operationalPeriodLength:       Long,
      registrationOperationalPeriod: Long,
      initialOperationalPeriod:      Long,
      initialKeyOpt:                 Option[SecretKeys.KesProduct],
      ref:                           Ref[F, (Long, Option[LongMap[KeyEvolverOut]])]
    )(implicit
      kesProduct: KesProduct,
      ed25519:    Ed25519
    ): KeyEvolverAlgebra[F] = { (slot: Slot) =>
      val operationalPeriod = slot / operationalPeriodLength
      ref.get.flatMap {
        case (`operationalPeriod`, keysOpt) =>
          keysOpt.flatMap(_.get(slot)).pure[F]
        case _ =>
          OptionT(consumeEvolvePersist(operationalPeriod, registrationOperationalPeriod, secureStore))
            .semiflatMap(childKesKey =>
              clock
                .epochOf(slot)
                .flatMap(vrfProof.ineligibleSlots)
                .map(_.toSet)
                .map(ineligibleSlots =>
                  prepareOperationalPeriodKeys(
                    childKesKey,
                    Vector
                      .unfold(slot)(s => if (s + slot < operationalPeriodLength) Some(s -> (s + 1)) else None)
                      .filterNot(ineligibleSlots)
                  )
                )
            )
            .map(outs => LongMap.from(outs.map(o => o.slot -> o)))
            .value
            .flatTap(newKeysOpt => ref.set((operationalPeriod, newKeysOpt)))
            .map(_.flatMap(_.get(slot)))
      }
    }

    /**
     * Consume the key (and any previous/lingering keys) closest to the given `currentOperationalPeriod`.  If the closest
     * key is not yet at the `currentOperationalPeriod`, it is evolved up to the `currentOperationalPeriod`.  The
     * key for `currentOperationalPeriod + 1` is constructed and saved to disk.  The `currentOperationalPeriod` key is
     * returned.
     */
    private def consumeEvolvePersist[F[_]: Monad](
      currentOperationalPeriod:      Long,
      registrationOperationalPeriod: Long,
      secureStore:                   SecureStore[F]
    ): F[Option[SecretKeys.KesProduct]] = {
      for {
        (toErase, latest) <- OptionT(
          secureStore.list
            .map(_.flatMap(t => Chain.fromOption(t.toLongOption)).filter(_ <= currentOperationalPeriod).sorted.initLast)
        )
        _       <- OptionT.liftF(toErase.map(_.toString).traverse(secureStore.erase))
        diskKey <- OptionT(secureStore.consume[SecretKeys.KesProduct](latest.toString))
        currentPeriodKey =
          if (latest === (currentOperationalPeriod - registrationOperationalPeriod)) diskKey
          else
            KesProduct.instance.update(
              diskKey,
              (currentOperationalPeriod - registrationOperationalPeriod - latest).toInt
            )
        _ <- OptionT.liftF(
          secureStore.write(
            (currentOperationalPeriod - registrationOperationalPeriod + 1).toString,
            KesProduct.instance.update(currentPeriodKey, 1)
          )
        )
      } yield currentPeriodKey
    }.value

    private def prepareOperationalPeriodKeys(kesChild: SecretKeys.KesProduct, slots: Vector[Slot])(implicit
      kesProduct:                                      KesProduct,
      ed25519:                                         Ed25519
    ): Vector[KeyEvolverOut] =
      slots.map { slot =>
        val sk = KeyInitializer[SecretKeys.Ed25519].random()
        KeyEvolverOut(slot, sk, kesProduct.sign(kesChild, ed25519.getVerificationKey(sk).bytes.data))
      }
  }
}
