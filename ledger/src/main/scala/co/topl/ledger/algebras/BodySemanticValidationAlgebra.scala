package co.topl.ledger.algebras

import co.topl.algebras.ContextualValidationAlgebra
import co.topl.ledger.models.{BodySemanticError, BodyValidationContext}

trait BodySemanticValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, BodySemanticError, co.topl.node.models.BlockBody, BodyValidationContext]
