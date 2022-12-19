package co.topl

package object typeclasses {

  object implicits
      extends ContainsEvidence.Instances
      with ContainsEvidence.ToContainsEvidenceOps
      with ContainsHeight.Instances
      with ContainsHeight.ToContainsHeightOps
      with ContainsSlot.Instances
      with ContainsSlot.ToContainsSlotOps
      with ContainsSlotId.Instances
      with ContainsSlotId.ToContainsSlotIdOps
      with ContainsParent.Instances
      with ContainsParent.ToContainsParentOps
      with ContainsTimestamp.Instances
      with ContainsTimestamp.ToContainsTimestampOps
      with ContainsTransactions.Instances
      with ContainsTransactions.ToContainsTransactionsOps
      with ContainsTransactionIds.Instances
      with ContainsTransactionIds.ToContainsTransactionIdsOps
      with RatioOps.Implicits
      with ContainsVerificationKey.Instances
      with ContainsVerificationKey.ToContainsVerificationKeyOps
      with Proposer.Instances
      with Proposer.Implicits
      with Proposer.ToProposerOps
      with Prover.Instances
      with Prover.ToProverOps
      with ShowInstances
      with EqInstances
      with SpendingAddressable.Instances
      with SpendingAddressable.ToSpendingAddressableOps
      with Prepend.Instances
      with NonEmpty.Instances
      with TransactionOps.Instances
      with IdentityOps
      with NumberOps
}
