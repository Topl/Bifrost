package co.topl.demo

import akka.actor.typed.ActorSystem
import akka.util.Timeout
import cats.Parallel
import cats.effect.Async
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras._
import co.topl.catsakka.FToFuture
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.LeaderElectionValidation.VrfConfig
import co.topl.consensus.algebras.{EtaCalculationAlgebra, LeaderElectionValidationAlgebra, LocalChainAlgebra}
import co.topl.crypto.signing.{Ed25519, Ed25519VRF, KesProduct}
import co.topl.interpreters.{AkkaSecureStore, StatsInterpreter}
import co.topl.ledger.algebras.{BodySemanticValidationAlgebra, BodySyntaxValidationAlgebra, MempoolAlgebra}
import co.topl.minting.algebras.PerpetualBlockMintAlgebra
import co.topl.minting._
import co.topl.models.{BlockHeaderV2, BlockV2, Transaction, TypedIdentifier}
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import java.nio.file.Files
import java.util.UUID

object DemoUtils {

  def createMint[F[_]: Async: Parallel: FToFuture](
    genesis:                 BlockV2,
    staker:                  Staker,
    clock:                   ClockAlgebra[F],
    etaCalculation:          EtaCalculationAlgebra[F],
    leaderElectionThreshold: LeaderElectionValidationAlgebra[F],
    localChain:              LocalChainAlgebra[F],
    mempool:                 MempoolAlgebra[F],
    headerStore:             Store[F, TypedIdentifier, BlockHeaderV2],
    fetchTransaction:        TypedIdentifier => F[Transaction],
    bodySyntaxValidation:    BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:  BodySemanticValidationAlgebra[F],
    state:                   ConsensusStateReader[F],
    ed25519VRFResource:      UnsafeResource[F, Ed25519VRF],
    kesProductResource:      UnsafeResource[F, KesProduct],
    ed25519Resource:         UnsafeResource[F, Ed25519],
    statsInterpreter:        Stats[F],
    operationalPeriodLength: Long
  )(implicit
    logger:    Logger[F],
    system:    ActorSystem[_],
    timeout:   Timeout,
    vrfConfig: VrfConfig
  ): F[PerpetualBlockMintAlgebra[F]] =
    for {
      _            <- Logger[F].info(show"Initializing staker key idx=0 address=${staker.address}")
      stakerKeyDir <- Async[F].blocking(Files.createTempDirectory(show"TetraDemoStaker${staker.address}"))
      secureStore  <- AkkaSecureStore.Eval.make[F](stakerKeyDir)
      _            <- secureStore.write(UUID.randomUUID().toString, staker.kesKey)
      vrfProofConstruction <- VrfProof.Eval.make[F](
        staker.vrfKey,
        clock,
        leaderElectionThreshold,
        ed25519VRFResource,
        vrfConfig
      )
      initialSlot  <- clock.globalSlot.map(_.max(0L))
      initialEpoch <- clock.epochOf(initialSlot)
      _            <- vrfProofConstruction.precomputeForEpoch(initialEpoch, genesis.headerV2.eligibilityCertificate.eta)
      operationalKeys <- OperationalKeys.FromSecureStore.make[F](
        secureStore = secureStore,
        clock = clock,
        vrfProof = vrfProofConstruction,
        etaCalculation,
        state,
        kesProductResource,
        ed25519Resource,
        genesis.headerV2.slotId,
        operationalPeriodLength = operationalPeriodLength,
        activationOperationalPeriod = 0L,
        staker.address,
        initialSlot = initialSlot
      )
      stakerVRFVK <- ed25519VRFResource.use(_.getVerificationKey(staker.vrfKey).pure[F])
      mint =
        BlockMint.Eval.make(
          Staking.Eval.make(
            staker.address,
            LeaderElectionMinting.Eval.make(
              stakerVRFVK,
              leaderElectionThreshold,
              vrfProofConstruction,
              statsInterpreter = StatsInterpreter.Noop.make[F],
              statsName = ""
            ),
            operationalKeys,
            VrfRelativeStakeMintingLookup.Eval.make(state, clock),
            etaCalculation,
            ed25519Resource,
            vrfProofConstruction,
            clock
          ),
          clock,
          statsInterpreter
        )
      perpetual <- PerpetualBlockMint.InAkkaStream
        .make(
          clock,
          mint,
          localChain,
          mempool,
          headerStore,
          fetchTransaction,
          bodySyntaxValidation,
          bodySemanticValidation
        )
    } yield perpetual

}
