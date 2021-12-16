package co.topl.typeclasses

import co.topl.models._

@simulacrum.typeclass
trait DionAddressable[T] {

  @simulacrum.op("dionAddress")
  def dionAddressOf(t: T): DionAddress
}

object DionAddressable {

  trait Instances {

    implicit def coinOutputHasDioanAddress(implicit networkPrefix: NetworkPrefix): DionAddressable[Transaction.CoinOutput] =
      new DionAddressable[Transaction.CoinOutput] {
        def dionAddressOf(t: Transaction.CoinOutput): DionAddress = t match {
          case b: Transaction.PolyOutput  => b.dionAddress
          case b: Transaction.ArbitOutput => b.dionAddress
          case b: Transaction.AssetOutput => b.dionAddress
        }
      }

    implicit def containsEvidenceDionAddressable[T: ContainsEvidence](implicit
      networkPrefix: NetworkPrefix
    ): DionAddressable[T] =
      new DionAddressable[T] {

        def dionAddressOf(t: T): DionAddress =
          DionAddress(
            networkPrefix,
            ContainsEvidence[T].typedEvidenceOf(t)
          )
      }
  }
}
