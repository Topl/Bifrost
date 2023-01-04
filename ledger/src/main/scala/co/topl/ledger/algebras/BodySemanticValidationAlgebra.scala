package co.topl.ledger.algebras

import co.topl.algebras.ContextualValidationAlgebra
import co.topl.ledger.models.{BodySemanticError, BodyValidationContext}
import co.topl.models.BlockBody

trait BodySemanticValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, BodySemanticError, BlockBody, BodyValidationContext]
