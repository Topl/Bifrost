package co.topl.ledger.algebras

import co.topl.algebras.{ContextlessValidation, ContextualValidationAlgebra}
import co.topl.ledger.models.BodySyntaxError
import co.topl.models.{BlockBodyV2, TypedIdentifier}

trait BodySyntaxValidationAlgebra[F[_]] extends ContextlessValidation[F, BodySyntaxError, BlockBodyV2]
