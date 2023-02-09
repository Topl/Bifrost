package co.topl.ledger.algebras

import co.topl.algebras.ContextlessValidationAlgebra
import co.topl.ledger.models.BodySyntaxError
import co.topl.node.models.BlockBody

trait BodySyntaxValidationAlgebra[F[_]] extends ContextlessValidationAlgebra[F, BodySyntaxError, BlockBody]
