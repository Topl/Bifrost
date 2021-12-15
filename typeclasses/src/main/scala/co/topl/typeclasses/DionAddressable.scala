package co.topl.typeclasses

import co.topl.models._

@simulacrum.typeclass
trait DionAddressable[T] {

  @simulacrum.op("dionAddress")
  def dionAddressOf(t: T): DionAddress
}

object DionAddressable {

  trait Instances {

    implicit val coinOutputDionAddressable: DionAddressable[Transaction.CoinOutput] = {
      case o: Transaction.PolyOutput  => o.dionAddress
      case o: Transaction.ArbitOutput => o.dionAddress
      case o: Transaction.AssetOutput => o.dionAddress
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
