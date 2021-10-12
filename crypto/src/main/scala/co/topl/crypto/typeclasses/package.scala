package co.topl.crypto

package object typeclasses {

  object implicits
      extends ContainsVerificationKey.Instances
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
//      with SoftDerivative.Instances
//      with SoftDerivative.ToSoftDerivativeOps
}
