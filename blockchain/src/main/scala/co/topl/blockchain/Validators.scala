package co.topl.blockchain

import cats.effect.{Async, Resource}
import cats.effect.implicits._
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.brambl.validation.TransactionAuthorizationInterpreter
import co.topl.brambl.validation.TransactionSyntaxInterpreter
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.consensus.algebras.BlockHeaderToBodyValidationAlgebra
import co.topl.consensus.algebras.BlockHeaderValidationAlgebra
import co.topl.consensus.algebras.ConsensusValidationStateAlgebra
import co.topl.consensus.algebras.EligibilityCacheAlgebra
import co.topl.consensus.algebras.EtaCalculationAlgebra
import co.topl.consensus.algebras.LeaderElectionValidationAlgebra
import co.topl.consensus.interpreters.BlockHeaderToBodyValidation
import co.topl.consensus.interpreters.BlockHeaderValidation
import co.topl.consensus.models.BlockId
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.algebras.{
  BodyAuthorizationValidationAlgebra,
  BodySemanticValidationAlgebra,
  BodySyntaxValidationAlgebra,
  BoxStateAlgebra,
  RegistrationAccumulatorAlgebra,
  TransactionSemanticValidationAlgebra
}
import co.topl.ledger.interpreters._
import co.topl.quivr.api.Verifier.instances.verifierInstance
import co.topl.typeclasses.implicits._
import co.topl.brambl.validation.algebras.TransactionAuthorizationVerifier

case class Validators[F[_]](
  header:                   BlockHeaderValidationAlgebra[F],
  headerToBody:             BlockHeaderToBodyValidationAlgebra[F],
  transactionSyntax:        TransactionSyntaxVerifier[F],
  transactionSemantics:     TransactionSemanticValidationAlgebra[F],
  transactionAuthorization: TransactionAuthorizationVerifier[F],
  bodySyntax:               BodySyntaxValidationAlgebra[F],
  bodySemantics:            BodySemanticValidationAlgebra[F],
  bodyAuthorization:        BodyAuthorizationValidationAlgebra[F],
  boxState:                 BoxStateAlgebra[F],
  registrationAccumulator:  RegistrationAccumulatorAlgebra[F]
)

object Validators {

  def make[F[_]: Async](
    cryptoResources:             CryptoResources[F],
    dataStores:                  DataStores[F],
    bigBangBlockId:              BlockId,
    eligibilityCache:            EligibilityCacheAlgebra[F],
    currentEventIdGetterSetters: CurrentEventIdGetterSetters[F],
    blockIdTree:                 ParentChildTree[F, BlockId],
    etaCalculation:              EtaCalculationAlgebra[F],
    consensusValidationState:    ConsensusValidationStateAlgebra[F],
    leaderElectionThreshold:     LeaderElectionValidationAlgebra[F],
    clockAlgebra:                ClockAlgebra[F]
  ): Resource[F, Validators[F]] =
    for {
      headerValidation <- BlockHeaderValidation
        .make[F](
          etaCalculation,
          consensusValidationState,
          leaderElectionThreshold,
          eligibilityCache,
          clockAlgebra,
          dataStores.headers,
          bigBangBlockId,
          cryptoResources.ed25519VRF,
          cryptoResources.kesProduct,
          cryptoResources.ed25519,
          cryptoResources.blake2b256
        )
        .flatMap(BlockHeaderValidation.WithCache.make[F](_))
        .toResource
      headerToBody <- BlockHeaderToBodyValidation.make().toResource
      boxState <- BoxState
        .make(
          currentEventIdGetterSetters.boxState.get(),
          dataStores.bodies.getOrRaise,
          dataStores.transactions.getOrRaise,
          blockIdTree,
          currentEventIdGetterSetters.boxState.set,
          dataStores.spendableBoxIds.pure[F]
        )
        .toResource
      transactionSyntaxValidation = TransactionSyntaxInterpreter.make[F]()
      transactionSemanticValidation <- TransactionSemanticValidation
        .make[F](dataStores.transactions.getOrRaise, boxState)
        .toResource
      transactionAuthorizationValidation = TransactionAuthorizationInterpreter.make[F]()
      bodySyntaxValidation <- BodySyntaxValidation
        .make[F](dataStores.transactions.getOrRaise, transactionSyntaxValidation)
        .toResource
      registrationAccumulator <- RegistrationAccumulator.make[F](
        currentEventIdGetterSetters.registrationAccumulator.get(),
        dataStores.bodies.getOrRaise,
        dataStores.transactions.getOrRaise,
        blockIdTree,
        currentEventIdGetterSetters.registrationAccumulator.set,
        dataStores.registrationAccumulator.pure[F]
      )
      bodySemanticValidation <- BodySemanticValidation
        .make[F](
          dataStores.transactions.getOrRaise,
          transactionSemanticValidation,
          registrationAccumulator
        )
        .toResource
      bodyAuthorizationValidation <- BodyAuthorizationValidation
        .make[F](
          dataStores.transactions.getOrRaise,
          transactionAuthorizationValidation
        )
        .toResource
    } yield Validators(
      headerValidation,
      headerToBody,
      transactionSyntaxValidation,
      transactionSemanticValidation,
      transactionAuthorizationValidation,
      bodySyntaxValidation,
      bodySemanticValidation,
      bodyAuthorizationValidation,
      boxState,
      registrationAccumulator
    )
}
