package co.topl.minting.interpreters

import cats.Traverse
import cats.data._
import cats.effect.Deferred
import cats.effect.implicits._
import cats.effect.{Async, MonadCancelThrow, Ref, Resource, Sync}
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.algebras.LeaderElectionValidationAlgebra
import co.topl.consensus.algebras.ConsensusValidationStateAlgebra
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.signing._
import co.topl.minting.algebras._
import co.topl.minting.models.OperationalKeyOut
import co.topl.models._
import co.topl.models.utility._
import co.topl.consensus.models._
import co.topl.crypto.models.SecretKeyKesProduct
import co.topl.typeclasses.implicits._
import com.google.common.primitives.Longs
import com.google.protobuf.ByteString
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.util.UUID

object OperationalKeyMaker {

  /**
   * Constructs an OperationalKeys interpreter using a SecureStore.
   *
   * @param activationOperationalPeriod The operational period number in which the staker becomes active
   * @param address                     The staker's address
   * @param etaCalculation An EtaCalculation interpreter is needed to determine the eta to use when determining VRF ineligibilities.
   * @param consensusState Used for the lookup of relative stake for VRF ineligibilities
   */
  def make[F[_]: Async](
    activationOperationalPeriod: Long,
    address:                     StakingAddress,
    vrfConfig:                   VrfConfig,
    secureStore:                 SecureStore[F],
    clock:                       ClockAlgebra[F],
    vrfCalculator:               VrfCalculatorAlgebra[F],
    leaderElection:              LeaderElectionValidationAlgebra[F],
    consensusState:              ConsensusValidationStateAlgebra[F],
    kesProductResource:          Resource[F, KesProduct],
    ed25519Resource:             Resource[F, Ed25519]
  ): Resource[F, OperationalKeyMakerAlgebra[F]] =
    for {
      // Delay further initialization until the activation period starts
      currentOperationalPeriod <- clock.globalOperationalPeriod.toResource
      _ <- Async[F]
        .whenA(activationOperationalPeriod > currentOperationalPeriod)(
          clock.operationalPeriodRange(activationOperationalPeriod).map(_.start).flatMap(clock.delayedUntilSlot)
        )
        .toResource
      stateRef                     <- Ref.of(none[(Long, Map[Long, Deferred[F, Option[OperationalKeyOut]]])]).toResource
      implicit0(logger: Logger[F]) <- Slf4jLogger.fromName("OperationalKeyMaker").toResource
      impl = new Impl[F](
        activationOperationalPeriod,
        address,
        vrfConfig,
        secureStore,
        clock,
        vrfCalculator,
        leaderElection,
        consensusState,
        kesProductResource,
        ed25519Resource,
        stateRef
      )
    } yield impl

  private class Impl[F[_]: Async: Logger](
    activationOperationalPeriod: Long,
    address:                     StakingAddress,
    vrfConfig:                   VrfConfig,
    secureStore:                 SecureStore[F],
    clock:                       ClockAlgebra[F],
    vrfCalculator:               VrfCalculatorAlgebra[F],
    leaderElection:              LeaderElectionValidationAlgebra[F],
    consensusState:              ConsensusValidationStateAlgebra[F],
    kesProductResource:          Resource[F, KesProduct],
    ed25519Resource:             Resource[F, Ed25519],
    stateRef:                    Ref[F, Option[(Long, Map[Long, Deferred[F, Option[OperationalKeyOut]]])]]
  ) extends OperationalKeyMakerAlgebra[F] {

    def operationalKeyForSlot(slot: Slot, parentSlotId: SlotId, eta: Eta): F[Option[OperationalKeyOut]] =
      clock
        .operationalPeriodOf(slot)
        .flatMap(operationalPeriod =>
          MonadCancelThrow[F].uncancelable(_ =>
            stateRef.get.flatMap {
              case Some((`operationalPeriod`, keys)) =>
                OptionT.fromOption[F](keys.get(slot)).flatMapF(_.get).value
              case _ =>
                OptionT(consensusState.operatorRelativeStake(parentSlotId.blockId, slot)(address))
                  .flatMapF(relativeStake =>
                    consumeEvolvePersist(
                      (operationalPeriod - activationOperationalPeriod).toInt,
                      slot,
                      relativeStake,
                      eta
                    )
                  )
                  .semiflatTap(newKeys => stateRef.set((operationalPeriod -> newKeys).some))
                  .flatTapNone(stateRef.set(none))
                  .subflatMap(_.get(slot))
                  .flatMapF(_.get)
                  .value
            }
          )
        )

    /**
     * Consume the current key from disk.  Exactly one key is expected; if 0 or more than is detected, an error is raised.
     * If they key is behind the `currentOperationalPeriod`, it is first updated to the `currentOperationalPeriod`.  The
     * key for `timeStep + 1` is constructed and saved to disk.
     */
    private[interpreters] def consumeEvolvePersist(
      timeStep:      Int,
      slot:          Slot,
      relativeStake: Ratio,
      eta:           Eta
    ): F[Option[Map[Slot, Deferred[F, Option[OperationalKeyOut]]]]] =
      MonadCancelThrow[F].uncancelable(_ =>
        (
          for {
            fileName <- OptionT(
              secureStore.list
                .ensure(new IllegalStateException("SecureStore is empty"))(_.nonEmpty)
                .ensure(new IllegalStateException("SecureStore contains multiple keys"))(_.length === 1)
                .map(_.headOption)
            )
            _       <- OptionT.liftF(Logger[F].info(show"Consuming key id=$fileName"))
            diskKey <- OptionT(secureStore.consume[SecretKeyKesProduct](fileName))
            latest <- OptionT.liftF(
              kesProductResource.use(kesProduct => Sync[F].delay(kesProduct.getCurrentStep(diskKey)))
            )
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
              else
                OptionT.liftF(kesProductResource.use(kesProduct => Sync[F].delay(kesProduct.update(diskKey, timeStep))))
            nextTimeStep = timeStep + 1
            onComplete = for {
              _ <- Sync[F].defer(Logger[F].info(show"Saving next key idx=$nextTimeStep"))
              updated <-
                kesProductResource.use(kesProduct => Sync[F].delay(kesProduct.update(currentPeriodKey, nextTimeStep)))
              _ <- secureStore.write(UUID.randomUUID().toString, updated)
            } yield ()
            res <- OptionT.liftF(
              prepareOperationalPeriodKeys(currentPeriodKey, slot, relativeStake, eta)(onComplete)
            )
          } yield res
        ).value
      )

    /**
     * Using some KES parent, construct the linear keys for the upcoming operational period.  A linear key is constructed
     * for each slot for which we _might_ be eligible for VRF.
     */
    private[interpreters] def prepareOperationalPeriodKeys(
      parentSK:      SecretKeyKesProduct,
      fromSlot:      Slot,
      relativeStake: Ratio,
      eta:           Eta
    )(onComplete: => F[Unit]): F[Map[Slot, Deferred[F, Option[OperationalKeyOut]]]] =
      for {
        epoch                  <- clock.epochOf(fromSlot)
        operationalPeriod      <- clock.operationalPeriodOf(fromSlot)
        operationalPeriodSlots <- clock.operationalPeriodRange(operationalPeriod).map(_.toList)
        _ <- Logger[F].info(
          show"Computing operational keys for" +
          show" epoch=$epoch" +
          show" eta=$eta" +
          show" range=${operationalPeriodSlots.head}..${operationalPeriodSlots.last}"
        )
        parentVK      <- kesProductResource.use(kesProduct => Sync[F].delay(kesProduct.getVerificationKey(parentSK)))
        deferredSlots <- operationalPeriodSlots.traverse(slot => Deferred[F, Option[OperationalKeyOut]].map((slot, _)))
        threshold     <- maximumThreshold(relativeStake)
        // Launch a background fiber which will create the child keys for the new operational period.
        // As each child key is created, the corresponding Deferred instance is completed.
        _ <- Async[F]
          .uncancelable(poll =>
            Async[F].cede >>
            // Allow cancelation of the child key creation
            poll(
              fulfillDeferredSlots(epoch, eta, threshold, parentSK, parentVK, operationalPeriodSlots)(
                deferredSlots
              ).void
            ) >>
            // But disable cancelation for the onComplete (evolve) action
            onComplete
          )
          .start
      } yield deferredSlots.toMap

    private def fulfillDeferredSlots[G[_]: Traverse](
      epoch:                  Epoch,
      eta:                    Eta,
      threshold:              Ratio,
      parentSK:               SecretKeyKesProduct,
      parentVK:               VerificationKeyKesProduct,
      operationalPeriodSlots: Iterable[Slot]
    )(deferredSlots: G[(Slot, Deferred[F, Option[OperationalKeyOut]])]) =
      deferredSlots.traverse { case (slot, deferred) =>
        potentiallyEligibleSlot(eta, slot, threshold)
          .ifM(
            ifTrue = prepareOperationalPeriodKey(parentSK, parentVK, slot).map(_.some),
            ifFalse = none[OperationalKeyOut].pure[F]
          )
          .flatMap(deferred.complete)
      } >> Logger[F].info(
        show"Finished computing operational keys for" +
        show" epoch=$epoch" +
        show" eta=$eta" +
        show" range=${operationalPeriodSlots.head}..${operationalPeriodSlots.last}"
      )

    /**
     * From some "parent" KES keypair, create a single child key for the given slot
     */
    private def prepareOperationalPeriodKey(
      parentSK: SecretKeyKesProduct,
      parentVK: VerificationKeyKesProduct,
      slot:     Slot
    ) =
      for {
        entropy      <- Sync[F].delay(Entropy.fromUuid(UUID.randomUUID()))
        childKeyPair <- ed25519Resource.use(ed => Sync[F].delay(ed.deriveKeyPairFromEntropy(entropy, None)))
        _            <- Async[F].cede
        message = childKeyPair.verificationKey.bytes ++ Longs.toByteArray(slot)
        parentSignature <- kesProductResource
          .use(kesProductScheme => Sync[F].delay(kesProductScheme.sign(parentSK, message)))
      } yield OperationalKeyOut(
        slot,
        ByteString.copyFrom(childKeyPair.verificationKey.bytes),
        ByteString.copyFrom(childKeyPair.signingKey.bytes),
        parentSignature,
        parentVK
      )

    /**
     * Determines the threshold at the VRF LDD Cutoff point.
     * @param relativeStake The operator's relative stake
     * @return a threshold at the LDD Cutoff Point
     */
    private def maximumThreshold(relativeStake: Ratio): F[Ratio] =
      leaderElection.getThreshold(relativeStake, vrfConfig.lddCutoff)

    /**
     * Determines if the operator *might* be eligible for the given slot,
     * based on the threshold at the LDD Cutoff.  If the operator is not
     * eligible at the cutoff slot, then it will never be eligible
     * @param eta Eta value
     * @param slot Test slot
     * @param threshold LDD Cutoff Threshold
     * @return true if potentially eligible, false if definitely ineligible
     */
    private def potentiallyEligibleSlot(eta: Eta, slot: Slot, threshold: Ratio) =
      vrfCalculator
        .rhoForSlot(slot, eta)
        .flatMap(leaderElection.isSlotLeaderForThreshold(threshold))
  }

}
