package co.topl.ledger.algebras

import co.topl.algebras.ContextualValidationAlgebra
import co.topl.ledger.models.{TransactionSemanticError, TransactionValidationContext}
import co.topl.models.Transaction

trait TransactionSemanticValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, TransactionSemanticError, Transaction, TransactionValidationContext]
