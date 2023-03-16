package co.topl.ledger.algebras

import co.topl.algebras.ContextualValidationAlgebra
import co.topl.ledger.models.{TransactionSemanticError, TransactionValidationContext}
import co.topl.brambl.models.transaction.IoTransaction

trait TransactionSemanticValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, TransactionSemanticError, IoTransaction, TransactionValidationContext]
