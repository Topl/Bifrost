package co.topl.typeclasses

import co.topl.models._

@simulacrum.typeclass
trait SpendingAddressable[T] {

  @simulacrum.op("spendingAddress")
  def spendingAddressOf(t: T): SpendingAddress
}

object SpendingAddressable {

  trait Instances {

    implicit def containsEvidenceSpendingAddressable[T: ContainsEvidence]: SpendingAddressable[T] =
      (t: T) => SpendingAddress(ContainsEvidence[T].typedEvidenceOf(t))
  }
}
