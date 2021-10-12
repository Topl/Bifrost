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
      with Identifiable.Instances
      with Identifiable.ToIdentifiableOps
      with RatioOps.Implicits
      with ShowInstances
      with EqInstances
}
