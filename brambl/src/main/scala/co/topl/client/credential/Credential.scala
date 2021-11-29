package co.topl.client.credential

import co.topl.models._
import co.topl.typeclasses._
import co.topl.typeclasses.implicits._

/**
 * An addressable entity for proving transactions
 */
trait Credential[Prop <: Proposition] {
  def proposition: Prop
  def prove(partiallyProven: Transaction): Transaction
  def address: DionAddress
}

object Credential {

  def apply[T, Prf <: Proof, Prop <: Proposition](
    t: T
  )(implicit
    prover:          Prover[T, Prf],
    proposer:        Proposer[T, Prop],
    dionAddressable: DionAddressable[T],
    networkPrefix:   NetworkPrefix
  ): Credential[Prop] =
    new Credential[Prop] {

      val proposition: Prop = proposer.propositionOf(t)

      val address: DionAddress = dionAddressable.dionAddressOf(t)

      def prove(partiallyProven: Transaction): Transaction = {
        val partialAttestation =
          proposition -> prover.proveWith(t, partiallyProven: Transaction)

        partiallyProven match {
          case tx: Transactions.Poly =>
            tx.copy(attestation = tx.attestation + partialAttestation)
          case tx: Transactions.Arbit =>
            tx.copy(attestation = tx.attestation + partialAttestation)
          case tx: Transactions.Asset =>
            tx.copy(attestation = tx.attestation + partialAttestation)
        }
      }
    }
}
