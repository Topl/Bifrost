package co.topl.ledger.algebras

import co.topl.algebras.ContextualValidationAlgebra
import co.topl.ledger.models.{TransactionSemanticError, TransactionValidationContext}
import co.topl.{models => legacyModels}
import legacyModels.Transaction

trait TransactionSemanticValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, TransactionSemanticError, Transaction, TransactionValidationContext]
