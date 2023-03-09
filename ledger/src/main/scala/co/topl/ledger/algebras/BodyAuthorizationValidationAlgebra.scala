package co.topl.ledger.algebras

import co.topl.algebras.ContextualValidationAlgebra
import co.topl.brambl.models.Datum
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.ledger.models.BodyAuthorizationError
import co.topl.node.models.BlockBody
import co.topl.quivr.runtime.DynamicContext

trait BodyAuthorizationValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[
      F,
      BodyAuthorizationError,
      BlockBody,
      IoTransaction => DynamicContext[F, String, Datum]
    ]
