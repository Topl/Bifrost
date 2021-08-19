package co.topl.ledger

import co.topl.models._
import co.topl.typeclasses.StatefullyValidatable
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

class LedgerStatefullyValidatable
    extends StatefullyValidatable[
      LedgerValidation.State,
      BlockHeaderV2,
      LedgerValidation.ValidatedBlockBody,
      LedgerValidation.Failure
    ] {

  override def validate(
    t:     BlockHeaderV2,
    state: LedgerValidation.State
  ): Either[LedgerValidation.Failure, LedgerValidation.ValidatedBlockBody] = ???
}

object LedgerValidation {

  sealed abstract class Failure

  object Failures {}

  trait State {
    def boxesFor(address: Address): Iterable[Box]
    def box(id:           TypedIdentifier): Option[Box]
  }

  @newtype class ValidatedBlockBody(body: BlockBodyV2)

  private[ledger] object ValidatedBlockBody {

    def apply(body: BlockBodyV2): ValidatedBlockBody =
      body.coerce
  }

}
