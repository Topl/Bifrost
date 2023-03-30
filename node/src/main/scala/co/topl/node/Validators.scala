package co.topl.node

import cats.effect.Async
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.brambl.validation.TransactionAuthorizationInterpreter
import co.topl.brambl.validation.TransactionSyntaxInterpreter
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.consensus.algebras.EligibilityCacheAlgebra
import co.topl.consensus.algebras.{
  BlockHeaderToBodyValidationAlgebra,
  BlockHeaderValidationAlgebra,
  ConsensusValidationStateAlgebra,
  EtaCalculationAlgebra,
  LeaderElectionValidationAlgebra
}
import co.topl.consensus.interpreters.EligibilityCache
import co.topl.consensus.interpreters.{BlockHeaderToBodyValidation, BlockHeaderValidation}
import co.topl.consensus.models.BlockId
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.algebras.{
  BodyAuthorizationValidationAlgebra,
  BodySemanticValidationAlgebra,
  BodySyntaxValidationAlgebra
}
import co.topl.ledger.interpreters._
import co.topl.quivr.api.Verifier.instances.verifierInstance
import co.topl.typeclasses.implicits._

case class Validators[F[_]](
  header:            BlockHeaderValidationAlgebra[F],
  headerToBody:      BlockHeaderToBodyValidationAlgebra[F],
  transactionSyntax: TransactionSyntaxVerifier[F],
  bodySyntax:        BodySyntaxValidationAlgebra[F],
  bodySemantics:     BodySemanticValidationAlgebra[F],
  bodyAuthorization: BodyAuthorizationValidationAlgebra[F]
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
  ): F[Validators[F]] =
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
        .flatMap(BlockHeaderValidation.WithCache.make[F](_, dataStores.headers, bigBangBlockId))
      headerToBody <- BlockHeaderToBodyValidation.make()
      boxState <- BoxState.make(
        currentEventIdGetterSetters.boxState.get(),
        dataStores.bodies.getOrRaise,
        dataStores.transactions.getOrRaise,
        blockIdTree,
        currentEventIdGetterSetters.boxState.set,
        dataStores.spendableBoxIds.pure[F]
      )
      transactionSyntaxValidation = TransactionSyntaxInterpreter.make[F]()
      transactionSemanticValidation <- TransactionSemanticValidation
        .make[F](dataStores.transactions.getOrRaise, boxState)
      transactionAuthorizationValidation = TransactionAuthorizationInterpreter.make[F]()
      bodySyntaxValidation <- BodySyntaxValidation
        .make[F](dataStores.transactions.getOrRaise, transactionSyntaxValidation)
      bodySemanticValidation <- BodySemanticValidation.make[F](
        dataStores.transactions.getOrRaise,
        transactionSemanticValidation
      )
      bodyAuthorizationValidation <- BodyAuthorizationValidation.make[F](
        dataStores.transactions.getOrRaise,
        transactionAuthorizationValidation
      )
    } yield Validators(
      headerValidation,
      headerToBody,
      transactionSyntaxValidation,
      bodySyntaxValidation,
      bodySemanticValidation,
      bodyAuthorizationValidation
    )
}
