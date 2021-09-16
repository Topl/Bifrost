package co.topl.algebras

import co.topl.models.{Bytes, Rho, Slot, SlotId}

trait TineAlgebra[F[_]] {
  def update(slotId:        SlotId, rho:       Rho): F[TineAlgebra[F]]
  def reorg(prefix:         Slot, tineAlgebra: TineAlgebra[F]): F[TineAlgebra[F]]
  def notSparsePast(prefix: Slot): F[Boolean]
  def orderedNonceData(min: Slot, max:         Slot, tine: Option[TineAlgebra[F]]): F[Bytes]
  def slotIdOf(slot:        Slot): F[Option[SlotId]]
  def rhoOf(slot:           Slot): F[Option[Rho]]
  def slotIdsAfter(start:   Slot, count:       Int): F[Seq[SlotId]]
  def lastActiveOf(slot:    Slot): F[Option[SlotId]]
  def countActiveSlots(): F[Int]
  def head(): F[SlotId]
  def oldest(): F[SlotId]
}
