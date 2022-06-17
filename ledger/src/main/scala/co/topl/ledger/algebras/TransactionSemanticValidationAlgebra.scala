package co.topl.ledger.algebras

import co.topl.algebras.ContextualValidationAlgebra
import co.topl.ledger.models.TransactionSemanticError
import co.topl.models.{Transaction, TypedIdentifier}

trait TransactionSemanticValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, TransactionSemanticError, Transaction, TypedIdentifier]
