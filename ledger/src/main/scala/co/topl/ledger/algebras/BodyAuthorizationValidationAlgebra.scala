package co.topl.ledger.algebras

import co.topl.algebras.ContextualValidationAlgebra
import co.topl.ledger.models.BodyAuthorizationError
import co.topl.models.{BlockBodyV2, TypedIdentifier}

trait BodyAuthorizationValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, BodyAuthorizationError, BlockBodyV2, TypedIdentifier]