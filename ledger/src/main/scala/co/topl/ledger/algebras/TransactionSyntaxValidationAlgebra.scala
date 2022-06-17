package co.topl.ledger.algebras

import co.topl.algebras.ContextlessValidationAlgebra
import co.topl.ledger.models.TransactionSyntaxError
import co.topl.models.Transaction

trait TransactionSyntaxValidationAlgebra[F[_]]
    extends ContextlessValidationAlgebra[F, TransactionSyntaxError, Transaction]
