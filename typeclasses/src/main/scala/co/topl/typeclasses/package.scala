package co.topl

package object typeclasses {

  object implicits
      extends ContainsEvidence.Instances
      with ContainsEvidence.ToContainsEvidenceOps
      with ContainsHeight.Instances
      with ContainsHeight.ToContainsHeightOps
      with ContainsParent.Instances
      with ContainsParent.ToContainsParentOps
      with ContainsTimestamp.Instances
      with ContainsTimestamp.ToContainsTimestampOps
      with ContainsTransactions.Instances
      with ContainsTransactions.ToContainsTransactionsOps
      with Identifiable.Instances
      with Identifiable.ToIdentifiableOps
      with RatioOps.Implicits
      with ContainsVerificationKey.Instances
      with ContainsVerificationKey.Implicits
      with Evolves.Instances
      with Evolves.ToEvolvesOps
      with KeyInitializer.Instances
      with KeyInitializer.ToKeyInitializerOps
      with ProofVerifier.Instances
      with ProofVerifier.Implicits
      with Proposer.Instances
      with Proposer.implicits
      with Prover.Instances
      with Prover.Implicits
      with Signable.Instances
      with Signable.ToSignableOps
      with ShowInstances
      with EqInstances
      with DionAddressable.Instances
      with DionAddressable.ToDionAddressableOps
}
