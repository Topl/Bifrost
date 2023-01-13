package co.topl.ledger.algebras

import co.topl.algebras.ContextlessValidationAlgebra
import co.topl.ledger.models.BodySyntaxError

trait BodySyntaxValidationAlgebra[F[_]] extends ContextlessValidationAlgebra[F, BodySyntaxError, co.topl.node.models.BlockBody]
