package co.topl.ledger.algebras

import co.topl.algebras.ContextualValidationAlgebra
import co.topl.ledger.models.TransactionAuthorizationError
import co.topl.models.{Transaction, TypedIdentifier}

trait TransactionAuthorizationValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, TransactionAuthorizationError, Transaction, TypedIdentifier]
