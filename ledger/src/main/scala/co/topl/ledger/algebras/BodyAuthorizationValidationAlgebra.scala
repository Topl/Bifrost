package co.topl.ledger.algebras

import co.topl.algebras.ContextualValidationAlgebra
import co.topl.ledger.models.BodyAuthorizationError
import co.topl.models.TypedIdentifier

trait BodyAuthorizationValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, BodyAuthorizationError, co.topl.node.models.BlockBody, TypedIdentifier]
