package co.topl.ledger

import co.topl.brambl.validation.algebras._
import co.topl.ledger.algebras._

trait Ledger[F[_]] {
  def transactionSyntaxValidation: TransactionSyntaxVerifier[F]
  def transactionSemanticValidation: TransactionSemanticValidationAlgebra[F]
  def transactionAuthorizationValidation: TransactionAuthorizationVerifier[F]
  def bodySyntaxValidation: BodySyntaxValidationAlgebra[F]
  def bodySemanticValidation: BodySemanticValidationAlgebra[F]
  def bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F]
  def mempool: MempoolAlgebra[F]
  def boxState: BoxStateAlgebra[F]
  def registrationAccumulator: RegistrationAccumulatorAlgebra[F]
  def transactionRewardCalculator: TransactionRewardCalculatorAlgebra
  def transactionCostCalculator: TransactionCostCalculator
}

case class LedgerImpl[F[_]](
  transactionSyntaxValidation:        TransactionSyntaxVerifier[F],
  transactionSemanticValidation:      TransactionSemanticValidationAlgebra[F],
  transactionAuthorizationValidation: TransactionAuthorizationVerifier[F],
  bodySyntaxValidation:               BodySyntaxValidationAlgebra[F],
  bodySemanticValidation:             BodySemanticValidationAlgebra[F],
  bodyAuthorizationValidation:        BodyAuthorizationValidationAlgebra[F],
  mempool:                            MempoolAlgebra[F],
  boxState:                           BoxStateAlgebra[F],
  registrationAccumulator:            RegistrationAccumulatorAlgebra[F],
  transactionRewardCalculator:        TransactionRewardCalculatorAlgebra,
  transactionCostCalculator:          TransactionCostCalculator
) extends Ledger[F]
