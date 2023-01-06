package co.topl

package object typeclasses {

  object implicits
      extends ContainsEvidence.Instances
      with ContainsEvidence.ToContainsEvidenceOps
      with ContainsTransactions.Instances
      with ContainsTransactions.ToContainsTransactionsOps
      with ContainsTransactionIds.Instances
      with ContainsTransactionIds.ToContainsTransactionIdsOps
      with ShowInstances
      with EqInstances
      with SpendingAddressable.Instances
      with SpendingAddressable.ToSpendingAddressableOps
      with Prepend.Instances
      with NonEmpty.Instances
      with TransactionOps.Instances
      with IdentityOps
}
