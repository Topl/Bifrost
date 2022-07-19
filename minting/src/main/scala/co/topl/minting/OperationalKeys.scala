package co.topl.minting

import cats._
import cats.data._
import cats.effect.Ref
import cats.effect.kernel.Concurrent
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.algebras.{ConsensusValidationStateAlgebra, EtaCalculationAlgebra}
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.signing._
import co.topl.minting.algebras._
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.implicits._
import com.google.common.primitives.Longs
import org.typelevel.log4cats.Logger

import java.util.UUID

object OperationalKeys {

  object FromSecureStore {

    /**
     * Constructs an OperationalKeys interpreter using a SecureStore.
     * @param etaCalculation An EtaCalculation interpreter is needed to determine the eta to use when determining VRF ineligibilities.
     * @param consensusState Used for the lookup of relative stake for VRF ineligibilities
     * @param parentSlotId The initial parentSlotId to use when launching the node and forming the first set of operational keys
     * @param operationalPeriodLength The number of slots in an operational period
     * @param activationOperationalPeriod The operational period number in which the staker becomes active
     * @param address The staker's address
     */
    def make[F[_]: Concurrent: Logger](
      secureStore:                 SecureStore[F],
      clock:                       ClockAlgebra[F],
      vrfProof:                    VrfProofAlgebra[F],
      etaCalculation:              EtaCalculationAlgebra[F],
      consensusState:              ConsensusValidationStateAlgebra[F],
      kesProductResource:          UnsafeResource[F, KesProduct],
      ed25519Resource:             UnsafeResource[F, Ed25519],
      parentSlotId:                SlotId,
      operationalPeriodLength:     Long,
      activationOperationalPeriod: Long,
      address:                     StakingAddresses.Operator,
      initialSlot:                 Slot
    ): F[OperationalKeysAlgebra[F]] =
      for {
        initialOperationalPeriod <- (initialSlot / operationalPeriodLength).pure[F]
        initialKeysOpt <-
          OptionT(consensusState.operatorRelativeStake(parentSlotId.blockId, initialSlot)(address))
            .flatMapF(relativeStake =>
              consumeEvolvePersist(
                (initialOperationalPeriod - activationOperationalPeriod).toInt,
                secureStore,
                prepareOperationalPeriodKeys(
                  _,
                  initialSlot,
                  parentSlotId,
                  operationalPeriodLength,
                  relativeStake,
                  clock,
                  vrfProof,
                  etaCalculation,
                  kesProductResource,
                  ed25519Resource
                ),
                kesProductResource
              )
            )
            .value
        ref <- Ref.of((initialOperationalPeriod, initialKeysOpt))
      } yield make[F](
        secureStore,
        clock,
        vrfProof,
        etaCalculation,
        consensusState,
        kesProductResource,
        ed25519Resource,
        operationalPeriodLength,
        activationOperationalPeriod,
        address,
        ref
      )

    def make[F[_]: MonadError[*[_], Throwable]: Logger](
      secureStore:                 SecureStore[F],
      clock:                       ClockAlgebra[F],
      vrfProof:                    VrfProofAlgebra[F],
      etaCalculation:              EtaCalculationAlgebra[F],
      consensusState:              ConsensusValidationStateAlgebra[F],
      kesProductResource:          UnsafeResource[F, KesProduct],
      ed25519Resource:             UnsafeResource[F, Ed25519],
      operationalPeriodLength:     Long,
      activationOperationalPeriod: Long,
      address:                     StakingAddresses.Operator,
      ref:                         Ref[F, (Long, Option[Map[Long, OperationalKeyOut]])]
    ): OperationalKeysAlgebra[F] = { (slot: Slot, parentSlotId: SlotId) =>
      val operationalPeriod = slot / operationalPeriodLength
      ref.get.flatMap {
        case (`operationalPeriod`, keysOpt) =>
          keysOpt.flatMap(_.get(slot)).pure[F]
        case _ =>
          OptionT(consensusState.operatorRelativeStake(parentSlotId.blockId, slot)(address))
            .flatMapF(relativeStake =>
              consumeEvolvePersist(
                (operationalPeriod - activationOperationalPeriod).toInt,
                secureStore,
                prepareOperationalPeriodKeys(
                  _,
                  slot,
                  parentSlotId,
                  operationalPeriodLength,
                  relativeStake,
                  clock,
                  vrfProof,
                  etaCalculation,
                  kesProductResource,
                  ed25519Resource
                ),
                kesProductResource
              )
            )
            .semiflatTap(newKeys => ref.set(operationalPeriod -> newKeys.some))
            .flatTapNone(ref.set(operationalPeriod -> None))
            .subflatMap(_.get(slot))
            .value
      }
    }

    /**
     * Consume the current key from disk.  Exactly one key is expected; if 0 or more than is detected, an error is raised.
     * If they key is behind the `currentOperationalPeriod`, it is first updated to the `currentOperationalPeriod`.  The
     * key for `timeStep + 1` is constructed and saved to disk.
     */
    private def consumeEvolvePersist[F[_]: MonadError[*[_], Throwable]: Logger, T](
      timeStep:           Int,
      secureStore:        SecureStore[F],
      use:                SecretKeys.KesProduct => F[T],
      kesProductResource: UnsafeResource[F, KesProduct]
    ): F[Option[T]] = {
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
        latest  <- OptionT.liftF(kesProductResource.use(_.getCurrentStep(diskKey).pure[F]))
        currentPeriodKey <-
          if (latest === timeStep) OptionT.pure[F](diskKey)
          else OptionT.liftF(kesProductResource.use(_.update(diskKey, timeStep.toInt).pure[F]))
        res <- OptionT.liftF(use(currentPeriodKey))
        nextTimeStep = timeStep + 1
        _       <- OptionT.liftF(Logger[F].info(show"Saving next key idx=$nextTimeStep"))
        updated <- OptionT.liftF(kesProductResource.use(_.update(currentPeriodKey, nextTimeStep).pure[F]))
        _ <- OptionT.liftF(
          secureStore.write(
            UUID.randomUUID().toString,
            updated
          )
        )
      } yield res
    }.value

    /**
     * Using some KES parent, construct the linear keys for the upcoming operational period.  A linear key is constructed
     * for each slot for which we _might_ be eligible for VRF.
     *
     * @param parentSlotId Used for Eta lookup when determining ineligible VRF slots
     */
    private def prepareOperationalPeriodKeys[F[_]: Monad: Logger](
      kesParent:               SecretKeys.KesProduct,
      fromSlot:                Slot,
      parentSlotId:            SlotId,
      operationalPeriodLength: Long,
      relativeStake:           Ratio,
      clock:                   ClockAlgebra[F],
      vrfProof:                VrfProofAlgebra[F],
      etaCalculation:          EtaCalculationAlgebra[F],
      kesProductResource:      UnsafeResource[F, KesProduct],
      ed25519Resource:         UnsafeResource[F, Ed25519]
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
        ineligibleSlots <- vrfProof.ineligibleSlots(epoch, eta, operationalPeriodSlots.some, relativeStake).map(_.toSet)
        slots = Vector
          .tabulate((operationalPeriodLength - (fromSlot % operationalPeriodLength)).toInt)(_ + fromSlot)
          .filterNot(ineligibleSlots)
        _    <- Logger[F].info(s"Preparing linear keys.  count=${slots.size}")
        outs <- prepareOperationalPeriodKeys(kesParent, slots, kesProductResource, ed25519Resource)
        mappedKeys = outs.map(o => o.slot -> o).toMap
      } yield mappedKeys

    /**
     * From some "parent" KES key, create several SKs for each slot in the given list of slots
     */
    private def prepareOperationalPeriodKeys[F[_]: Monad](
      kesParent:          SecretKeys.KesProduct,
      slots:              Vector[Slot],
      kesProductResource: UnsafeResource[F, KesProduct],
      ed25519Resource:    UnsafeResource[F, Ed25519]
    ): F[Vector[OperationalKeyOut]] =
      ed25519Resource
        .use(ed =>
          List.fill(slots.size)(ed.deriveKeyPairFromEntropy(Entropy.fromUuid(UUID.randomUUID()), None)).pure[F]
        )
        .flatMap(children =>
          kesProductResource.use { kesProductScheme =>
            val parentVK = kesProductScheme.getVerificationKey(kesParent)
            slots
              .zip(children)
              .map { case (slot, (childSK, childVK)) =>
                val parentSignature =
                  kesProductScheme.sign(
                    kesParent,
                    (childVK.bytes.data ++ Bytes(Longs.toByteArray(slot)))
                  )
                OperationalKeyOut(slot, childSK, parentSignature, parentVK)
              }
              .pure[F]
          }
        )
  }
}
