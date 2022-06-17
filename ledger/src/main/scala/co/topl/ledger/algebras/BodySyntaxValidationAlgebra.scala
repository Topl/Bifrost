package co.topl.ledger.algebras

import co.topl.algebras.ContextlessValidationAlgebra
import co.topl.ledger.models.BodySyntaxError
import co.topl.models.BlockBodyV2

trait BodySyntaxValidationAlgebra[F[_]] extends ContextlessValidationAlgebra[F, BodySyntaxError, BlockBodyV2]
