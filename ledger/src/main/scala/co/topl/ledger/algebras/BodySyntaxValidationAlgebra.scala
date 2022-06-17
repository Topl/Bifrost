package co.topl.ledger.algebras

import co.topl.algebras.ContextualValidationAlgebra
import co.topl.ledger.models.BodySyntaxError
import co.topl.models.{BlockBodyV2, TypedIdentifier}

trait BodySyntaxValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, BodySyntaxError, BlockBodyV2, TypedIdentifier]
