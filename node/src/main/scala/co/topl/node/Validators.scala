package co.topl.node

import cats.effect.Async
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.blockchain.algebras.BlockHeaderToBodyValidationAlgebra
import co.topl.blockchain.interpreters.BlockHeaderToBodyValidation
import co.topl.consensus.algebras.{
  BlockHeaderValidationAlgebra,
  ConsensusValidationStateAlgebra,
  EtaCalculationAlgebra,
  LeaderElectionValidationAlgebra
}
import co.topl.consensus.interpreters.BlockHeaderValidation
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.algebras.{
  BodyAuthorizationValidationAlgebra,
  BodySemanticValidationAlgebra,
  BodySyntaxValidationAlgebra,
  TransactionSyntaxValidationAlgebra
}
import co.topl.ledger.interpreters._
import co.topl.models.TypedIdentifier
import co.topl.typeclasses.implicits._

case class Validators[F[_]](
  header:            BlockHeaderValidationAlgebra[F],
  headerToBody:      BlockHeaderToBodyValidationAlgebra[F],
  transactionSyntax: TransactionSyntaxValidationAlgebra[F],
  bodySyntax:        BodySyntaxValidationAlgebra[F],
  bodySemantics:     BodySemanticValidationAlgebra[F],
  bodyAuthorization: BodyAuthorizationValidationAlgebra[F]
)

object Validators {

  def make[F[_]: Async](
    cryptoResources:             CryptoResources[F],
    dataStores:                  DataStores[F],
    currentEventIdGetterSetters: CurrentEventIdGetterSetters[F],
    blockIdTree:                 ParentChildTree[F, TypedIdentifier],
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
          clockAlgebra,
          cryptoResources.ed25519VRF,
          cryptoResources.kesProduct,
          cryptoResources.ed25519,
          cryptoResources.blake2b256
        )
        .flatMap(BlockHeaderValidation.WithCache.make[F](_, dataStores.headers))
      headerToBody <- BlockHeaderToBodyValidation.make()
      boxState <- BoxState.make(
        currentEventIdGetterSetters.boxState.get(),
        dataStores.bodies.getOrRaise,
        dataStores.transactions.getOrRaise,
        blockIdTree,
        currentEventIdGetterSetters.boxState.set,
        dataStores.spendableBoxIds.pure[F]
      )
      transactionSyntaxValidation <- TransactionSyntaxValidation.make[F]
      transactionSemanticValidation <- TransactionSemanticValidation
        .make[F](dataStores.transactions.getOrRaise, boxState)
      transactionAuthorizationValidation <- TransactionAuthorizationValidation.make[F](
        cryptoResources.blake2b256,
        cryptoResources.ed25519,
        dataStores.slotData.getOrRaise
      )
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
