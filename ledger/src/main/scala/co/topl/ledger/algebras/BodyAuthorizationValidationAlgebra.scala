package co.topl.ledger.algebras

import co.topl.algebras.ContextualValidationAlgebra
import co.topl.consensus.models.BlockId
import co.topl.ledger.models.BodyAuthorizationError
import co.topl.node.models.BlockBody

trait BodyAuthorizationValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, BodyAuthorizationError, BlockBody, BlockId]
