package co.topl.typeclasses

import co.topl.models.{BoxReference, Proof, Proposition, Transaction}

import scala.collection.immutable.ListMap

object TransactionOps {

  trait Instances {

    implicit class TransactionSupport(transaction: Transaction) {

      def unproven: Transaction.Unproven =
        Transaction.Unproven(
          transaction.inputs.keys.toList,
          transaction.feeOutput,
          transaction.coinOutputs,
          transaction.fee,
          transaction.timestamp,
          transaction.data,
          transaction.minting
        )
    }

    implicit class UnprovenTransactionSupport(unproven: Transaction.Unproven) {

      def prove(prove: BoxReference => (Proposition, Proof)) =
        Transaction(
          ListMap.empty ++ unproven.inputs.map(boxRef => boxRef -> prove(boxRef)),
          unproven.feeOutput,
          unproven.coinOutputs,
          unproven.fee,
          unproven.timestamp,
          unproven.data,
          unproven.minting
        )
    }
  }

  object instances extends Instances
}
