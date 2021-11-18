package co.topl.client.credential

import co.topl.models._
import co.topl.typeclasses._
import co.topl.typeclasses.implicits._

/**
 * An addressable entity for proving transactions
 */
trait Credential[Prop <: Proposition] {
  def proposition: Prop
  def prove(unsigned: UnprovenTransaction): Transaction
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

      def prove(unsigned: UnprovenTransaction): Transaction = {
        val attestation: Attestation = Attestation(
          proposition -> prover.proveWith(t, unsigned)
        )
        unsigned match {
          case UnprovenTransactions.Poly(input, feeOutput, coinOutputs, fee, timestamp, data, minting) =>
            Transactions.Poly(input, feeOutput, coinOutputs, attestation, fee, timestamp, data, minting)
          case UnprovenTransactions.Arbit(input, feeOutput, coinOutputs, fee, timestamp, data, minting) =>
            Transactions.Arbit(input, feeOutput, coinOutputs, attestation, fee, timestamp, data, minting)
          case UnprovenTransactions.Asset(input, feeOutput, coinOutputs, fee, timestamp, data, minting) =>
            Transactions.Asset(input, feeOutput, coinOutputs, attestation, fee, timestamp, data, minting)
        }
      }
    }
}
