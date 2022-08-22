package co.topl.typeclasses

import cats.data.Chain
import cats.implicits._
import co.topl.models.utility.HasLength.instances.bigIntLength
import co.topl.models.utility.Sized
import co.topl.models.{Box, Proof, Proposition, Transaction}

object TransactionOps {

  trait Instances {

    implicit class TransactionSupport(transaction: Transaction) {

      def unproven: Transaction.Unproven =
        Transaction
          .Unproven(
            transaction.inputs.map(i => Transaction.Unproven.Input(i.boxId, i.proposition, i.value)),
            transaction.outputs,
            transaction.schedule,
            transaction.data
          )

      def unclaimedInputValues: Chain[Box.Value] = {
        val poly = transaction.outputs
          .collect { case Transaction.Output(_, Box.Values.Poly(value), _) =>
            value.data
          }
          .toIterable
          .sum
        val arbit = transaction.outputs
          .collect { case Transaction.Output(_, Box.Values.Arbit(value), _) =>
            value.data
          }
          .toIterable
          .sum
        // TODO: Asset values?
        Chain(Box.Values.Poly(Sized.maxUnsafe(poly)), Box.Values.Arbit(Sized.maxUnsafe(arbit)))
      }
    }

    implicit class UnprovenTransactionSupport(unproven: Transaction.Unproven) {

      def prove(prove: Proposition => Proof) =
        Transaction(
          unproven.inputs.map(i => Transaction.Input(i.boxId, i.proposition, prove(i.proposition), i.value)),
          unproven.outputs,
          unproven.chronology,
          unproven.data
        )
    }
  }

  object instances extends Instances
}
