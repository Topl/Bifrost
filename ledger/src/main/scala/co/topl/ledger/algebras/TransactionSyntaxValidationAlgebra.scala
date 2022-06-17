package co.topl.ledger.algebras

import co.topl.algebras.ContextlessValidation
import co.topl.ledger.models.TransactionSyntaxError
import co.topl.models.Transaction

trait TransactionSyntaxValidationAlgebra[F[_]] extends ContextlessValidation[F, TransactionSyntaxError, Transaction]
