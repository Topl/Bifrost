package co.topl.modifier.transaction.builder

import co.topl.attestation.Address
import co.topl.utils.Int128
import co.topl.utils.StringDataTypes.Latin1Data

import scala.language.implicitConversions

trait TransferBlueprint[T, Failure, Transfer] {

  def compileBlueprint(
    blueprint:      T,
    availableBoxes: TokenBoxes,
    changeAddress:  Address,
    fee:            Int128,
    data:           Option[Latin1Data]
  ): Either[Failure, Transfer]
}

object TransferBlueprint {

  class Ops[T](private val instance: T) {

    def compileTransfer[Failure, Transfer](
      availableBoxes:             TokenBoxes,
      senders:                    List[Address],
      changeAddress:              Address,
      fee:                        Int128,
      data:                       Option[Latin1Data]
    )(implicit transferBlueprint: TransferBlueprint[T, Failure, Transfer]): Either[Failure, Transfer] =
      transferBlueprint.compileBlueprint(instance, availableBoxes, changeAddress, fee, data)
  }

  trait ToOps {
    implicit def toTransferBlueprintOps[T](target: T): Ops[T] = new Ops(target)
  }

  trait Implicits extends ToOps

  object implicits extends Implicits
}
