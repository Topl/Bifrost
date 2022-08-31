package co.topl.node

import cats.effect.Async
import cats.implicits._
import co.topl.consensus.BlockHeaderValidation
import co.topl.consensus.algebras.{
  BlockHeaderValidationAlgebra,
  ConsensusValidationStateAlgebra,
  EtaCalculationAlgebra,
  LeaderElectionValidationAlgebra
}
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.algebras.{
  BodyAuthorizationValidationAlgebra,
  BodySemanticValidationAlgebra,
  BodySyntaxValidationAlgebra,
  TransactionSyntaxValidationAlgebra
}
import co.topl.ledger.interpreters._
import co.topl.models.{BlockHeaderV2, TypedIdentifier}
import co.topl.typeclasses.implicits._

case class Validators[F[_]](
  header:            BlockHeaderValidationAlgebra[F],
  transactionSyntax: TransactionSyntaxValidationAlgebra[F],
  bodySyntax:        BodySyntaxValidationAlgebra[F],
  bodySemantics:     BodySemanticValidationAlgebra[F],
  bodyAuthorization: BodyAuthorizationValidationAlgebra[F]
)

object Validators {

  def make[F[_]: Async](
    bigBangHeader:            BlockHeaderV2,
    cryptoResources:          CryptoResources[F],
    dataStores:               DataStores[F],
    blockIdTree:              ParentChildTree[F, TypedIdentifier],
    etaCalculation:           EtaCalculationAlgebra[F],
    consensusValidationState: ConsensusValidationStateAlgebra[F],
    leaderElectionThreshold:  LeaderElectionValidationAlgebra[F]
  ): F[Validators[F]] =
    for {
      headerValidation <- BlockHeaderValidation.Eval
        .make[F](
          etaCalculation,
          consensusValidationState,
          leaderElectionThreshold,
          cryptoResources.ed25519VRF,
          cryptoResources.kesProduct,
          cryptoResources.ed25519,
          cryptoResources.blake2b256
        )
        .flatMap(BlockHeaderValidation.WithCache.make[F](_, dataStores.headers))
      boxState <- BoxState.make(
        bigBangHeader.parentHeaderId.pure[F],
        dataStores.bodies.getOrRaise,
        dataStores.transactions.getOrRaise,
        blockIdTree,
        dataStores.spendableBoxIds.pure[F]
      )
      transactionSyntaxValidation <- TransactionSyntaxValidation.make[F]
      transactionAuthorizationValidation <- TransactionAuthorizationValidation.make[F](
        cryptoResources.blake2b256,
        cryptoResources.curve25519,
        cryptoResources.ed25519,
        cryptoResources.extendedEd25519,
        dataStores.slotData.getOrRaise
      )
      bodySyntaxValidation <- BodySyntaxValidation
        .make[F](dataStores.transactions.getOrRaise, transactionSyntaxValidation)
      bodySemanticValidation <- BodySemanticValidation.make[F](
        dataStores.transactions.getOrRaise,
        boxState,
        boxState => TransactionSemanticValidation.make[F](dataStores.transactions.getOrRaise, boxState)
      )
      bodyAuthorizationValidation <- BodyAuthorizationValidation.make[F](
        dataStores.transactions.getOrRaise,
        transactionAuthorizationValidation
      )
    } yield Validators(
      headerValidation,
      transactionSyntaxValidation,
      bodySyntaxValidation,
      bodySemanticValidation,
      bodyAuthorizationValidation
    )
}
