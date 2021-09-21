package co.topl.crypto

package object typeclasses {

  object implicits
      extends ContainsVerificationKey.Instances
      with Evolves.Instances
      with Evolves.ToEvolvesOps
      with KeyInitializer.Instances
      with KeyInitializer.ToKeyInitializerOps
      with ProofVerifier.Instances
      with ProofVerifier.Implicits
      with Proposes.Instances
      with Proposes.implicits
      with Proves.Instances
      with Signable.Instances
      with Signable.ToSignableOps
      with KesCertifies.Instances
      with KesCertifies.ToKesCertifiesOps
      with SoftDerivative.Instances
      with SoftDerivative.ToSoftDerivativeOps
}
