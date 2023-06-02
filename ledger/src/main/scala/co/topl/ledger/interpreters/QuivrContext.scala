package co.topl.ledger.interpreters

import cats.effect.Sync
import cats.implicits._
import co.topl.brambl.Context
import co.topl.brambl.models._
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockHeader
import co.topl.models.Slot
import co.topl.quivr.runtime.DynamicContext

object QuivrContext {

  def forConstructedBlock[F[_]: Sync](
    header:      BlockHeader,
    transaction: IoTransaction
  ): DynamicContext[F, String, Datum] =
    Context(
      transaction,
      header.slot,
      {
        case "header" => Datum().withHeader(Datum.Header(Event.Header(header.height))).some
        case _        => None
      }
    )

  def forProposedBlock[F[_]: Sync](
    height:      Long,
    slot:        Slot,
    transaction: IoTransaction
  ): DynamicContext[F, String, Datum] =
    Context(
      transaction,
      slot,
      {
        case "header" =>
          Datum().withHeader(Datum.Header(Event.Header(height))).some
        case _ =>
          None
      }
    )
}
