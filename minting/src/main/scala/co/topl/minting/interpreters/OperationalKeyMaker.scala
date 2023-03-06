package co.topl.minting.interpreters

import cats._
import cats.data._
import cats.effect.{Async, MonadCancelThrow, Ref, Sync}
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.algebras.{ConsensusValidationStateAlgebra, EtaCalculationAlgebra}
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.signing._
import co.topl.minting.algebras._
import co.topl.minting.models.OperationalKeyOut
import co.topl.models._
import co.topl.models.utility._
import co.topl.consensus.models.CryptoConsensusMorphismInstances._
import co.topl.consensus.models._
import co.topl.crypto.models.SecretKeyKesProduct
import co.topl.typeclasses.implicits._
import com.google.common.primitives.Longs
import com.google.protobuf.ByteString
import org.typelevel.log4cats.Logger

import java.util.UUID

object OperationalKeyMaker {

  /**
   * Constructs an OperationalKeys interpreter using a SecureStore.
   *
   * @param parentSlotId The initial parentSlotId to use when launching the node and forming the first set of operational keys
   * @param operationalPeriodLength     The number of slots in an operational period
   * @param activationOperationalPeriod The operational period number in which the staker becomes active
   * @param address                     The staker's address
   * @param etaCalculation An EtaCalculation interpreter is needed to determine the eta to use when determining VRF ineligibilities.
   * @param consensusState Used for the lookup of relative stake for VRF ineligibilities
   */
  def make[F[_]: Async: Parallel: Logger](
    initialSlot:                 Slot,
    parentSlotId:                SlotId,
    operationalPeriodLength:     Long,
    activationOperationalPeriod: Long,
    address:                     StakingAddress,
    secureStore:                 SecureStore[F],
    clock:                       ClockAlgebra[F],
    vrfCalculator:               VrfCalculatorAlgebra[F],
    etaCalculation:              EtaCalculationAlgebra[F],
    consensusState:              ConsensusValidationStateAlgebra[F],
    kesProductResource:          UnsafeResource[F, KesProduct],
    ed25519Resource:             UnsafeResource[F, Ed25519]
  ): F[OperationalKeyMakerAlgebra[F]] =
    for {
      initialOperationalPeriod <- (initialSlot / operationalPeriodLength).pure[F]
      stateRef                 <- Ref.of((initialOperationalPeriod, none[Map[Long, OperationalKeyOut]]))
      impl = new Impl[F](
        operationalPeriodLength,
        activationOperationalPeriod,
        address,
        secureStore,
        clock,
        vrfCalculator,
        etaCalculation,
        consensusState,
        kesProductResource,
        ed25519Resource,
        stateRef
      )
      initialKeysOpt <-
        OptionT(consensusState.operatorRelativeStake(parentSlotId.blockId, initialSlot)(address))
          .flatMapF(relativeStake =>
            impl.consumeEvolvePersist(
              (initialOperationalPeriod - activationOperationalPeriod).toInt,
              impl.prepareOperationalPeriodKeys(_, initialSlot, parentSlotId, relativeStake)
            )
          )
          .value
      _ <- stateRef.set((initialOperationalPeriod, initialKeysOpt))
    } yield impl

  private class Impl[F[_]: Sync: Parallel: Logger](
    operationalPeriodLength:     Long,
    activationOperationalPeriod: Long,
    address:                     StakingAddress,
    secureStore:                 SecureStore[F],
    clock:                       ClockAlgebra[F],
    vrfCalculator:               VrfCalculatorAlgebra[F],
    etaCalculation:              EtaCalculationAlgebra[F],
    consensusState:              ConsensusValidationStateAlgebra[F],
    kesProductResource:          UnsafeResource[F, KesProduct],
    ed25519Resource:             UnsafeResource[F, Ed25519],
    stateRef:                    Ref[F, (Long, Option[Map[Long, OperationalKeyOut]])]
  ) extends OperationalKeyMakerAlgebra[F] {

    def operationalKeyForSlot(slot: Slot, parentSlotId: SlotId): F[Option[OperationalKeyOut]] = {
      val operationalPeriod = slot / operationalPeriodLength
      MonadCancelThrow[F].uncancelable(_ =>
        stateRef.get.flatMap {
          case (`operationalPeriod`, keysOpt) =>
            keysOpt.flatMap(_.get(slot)).pure[F]
          case _ =>
            OptionT(consensusState.operatorRelativeStake(parentSlotId.blockId, slot)(address))
              .flatMapF(relativeStake =>
                consumeEvolvePersist(
                  (operationalPeriod - activationOperationalPeriod).toInt,
                  prepareOperationalPeriodKeys(_, slot, parentSlotId, relativeStake)
                )
              )
              .semiflatTap(newKeys => stateRef.set(operationalPeriod -> newKeys.some))
              .flatTapNone(stateRef.set(operationalPeriod -> None))
              .subflatMap(_.get(slot))
              .value
        }
      )
    }

    /**
     * Consume the current key from disk.  Exactly one key is expected; if 0 or more than is detected, an error is raised.
     * If they key is behind the `currentOperationalPeriod`, it is first updated to the `currentOperationalPeriod`.  The
     * key for `timeStep + 1` is constructed and saved to disk.
     */
    private[interpreters] def consumeEvolvePersist[T](
      timeStep: Int,
      use:      SecretKeyKesProduct => F[T]
    ): F[Option[T]] =
      MonadCancelThrow[F].uncancelable(_ =>
        (
          for {
            fileName <- OptionT.liftF(secureStore.list.flatMap {
              case Chain(fileName) => fileName.pure[F]
              case _ =>
                MonadError[F, Throwable].raiseError[String](
                  new IllegalStateException("SecureStore contained 0 or multiple keys")
                )
            })
            _       <- OptionT.liftF(Logger[F].info(show"Consuming key id=$fileName"))
            diskKey <- OptionT(secureStore.consume[SecretKeyKesProduct](fileName))
            latest  <- OptionT.liftF(kesProductResource.use(_.getCurrentStep(diskKey).pure[F]))
            currentPeriodKey <-
              if (latest === timeStep) OptionT.pure[F](diskKey)
              else if (latest > timeStep)
                OptionT
                  .none[F, SecretKeyKesProduct]
                  .flatTapNone(
                    Logger[F].info(
                      show"Persisted key timeStep=$latest is greater than current timeStep=$timeStep." +
                      show"  Re-persisting original key."
                    ) >>
                    secureStore.write(fileName, diskKey)
                  )
              else OptionT.liftF(kesProductResource.use(_.update(diskKey, timeStep.toInt).pure[F]))
            res <- OptionT.liftF(use(currentPeriodKey))
            nextTimeStep = timeStep + 1
            _       <- OptionT.liftF(Logger[F].info(show"Saving next key idx=$nextTimeStep"))
            updated <- OptionT.liftF(kesProductResource.use(_.update(currentPeriodKey, nextTimeStep).pure[F]))
            _       <- OptionT.liftF(secureStore.write(UUID.randomUUID().toString, updated))
          } yield res
        ).value
      )

    /**
     * Using some KES parent, construct the linear keys for the upcoming operational period.  A linear key is constructed
     * for each slot for which we _might_ be eligible for VRF.
     *
     * @param parentSlotId Used for Eta lookup when determining ineligible VRF slots
     */
    private[interpreters] def prepareOperationalPeriodKeys(
      kesParent:     SecretKeyKesProduct,
      fromSlot:      Slot,
      parentSlotId:  SlotId,
      relativeStake: Ratio
    ): F[Map[Long, OperationalKeyOut]] =
      for {
        epoch <- clock.epochOf(fromSlot)
        eta   <- etaCalculation.etaToBe(parentSlotId, fromSlot)
        operationalPeriod = fromSlot / operationalPeriodLength
        operationalPeriodSlots = Range.Long(
          operationalPeriod * operationalPeriodLength,
          (operationalPeriod + 1) * operationalPeriodLength,
          1L
        )
        _ <- Logger[F].info(
          show"Computing ineligible slots for" +
          show" epoch=$epoch" +
          show" eta=$eta" +
          show" range=${operationalPeriodSlots.start}..${operationalPeriodSlots.last}"
        )
        ineligibleSlots <- vrfCalculator
          .ineligibleSlots(epoch, eta, operationalPeriodSlots.some, relativeStake)
          .map(_.toSet)
        slots = Vector
          .tabulate((operationalPeriodLength - (fromSlot % operationalPeriodLength)).toInt)(_ + fromSlot)
          .filterNot(ineligibleSlots)
        _    <- Logger[F].info(s"Preparing linear keys.  count=${slots.size}")
        outs <- prepareOperationalPeriodKeys(kesParent, slots)
        mappedKeys = outs.map(o => o.slot -> o).toMap
      } yield mappedKeys

    /**
     * From some "parent" KES key, create several SKs for each slot in the given list of slots
     */
    private[interpreters] def prepareOperationalPeriodKeys(
      kesParent: SecretKeyKesProduct,
      slots:     Vector[Slot]
    ): F[Vector[OperationalKeyOut]] =
      Sync[F]
        .delay(List.fill(slots.size)(()))
        .flatMap(
          _.parTraverse(_ =>
            ed25519Resource
              .use(ed =>
                Sync[F].delay(
                  ed.deriveKeyPairFromEntropy(Entropy.fromUuid(UUID.randomUUID()), None)
                )
              )
              .map { case (sk, vk) => (sk: ByteString, vk: ByteString) }
          )
        )
        .flatMap(children =>
          kesProductResource
            .use(r => Sync[F].delay(r.getVerificationKey(kesParent)))
            .flatMap(parentVK =>
              slots
                .zip(children)
                .parTraverse { case (slot, (childSK, childVK)) =>
                  kesProductResource.use(kesProductScheme =>
                    Sync[F]
                      .delay {
                        kesProductScheme.sign(
                          kesParent,
                          childVK.concat(ByteString.copyFrom(Longs.toByteArray(slot))).toByteArray
                        )
                      }
                      .flatMap(parentSignature =>
                        (for {
                          signature       <- EitherT(parentSignature.toF[F, SignatureKesProduct])
                          verificationKey <- EitherT(parentVK.toF[F, VerificationKeyKesProduct])
                        } yield (signature, verificationKey))
                          .getOrRaise(new IllegalStateException("Invalid model conversion"))
                      )
                      .map { case (parentSignature, parentVK) =>
                        OperationalKeyOut(slot, childVK, childSK, parentSignature, parentVK)
                      }
                  )
                }
            )
        )
  }

}
