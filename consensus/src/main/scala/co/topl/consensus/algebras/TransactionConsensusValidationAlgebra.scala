package co.topl.consensus.algebras

import co.topl.algebras.ContextualValidationAlgebra
import co.topl.consensus.models.TransactionConsensusError
import co.topl.models.{Transaction, TypedIdentifier}

trait TransactionConsensusValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, TransactionConsensusError, Transaction, TypedIdentifier]
