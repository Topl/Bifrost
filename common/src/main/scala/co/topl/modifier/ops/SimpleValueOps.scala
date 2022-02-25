package co.topl.modifier.ops

import cats.implicits._
import co.topl.attestation.Address
import co.topl.attestation.ops.AddressOps.ToDionAddressFailure
import co.topl.models.{DionAddress, Transaction}
import co.topl.modifier.box.SimpleValue
import co.topl.attestation.ops.AddressOps.implicits._
import co.topl.models.utility.HasLength.instances.bigIntLength
import co.topl.models.utility.{Lengths, Sized}

import scala.language.implicitConversions

class SimpleValueOps(val simpleValue: SimpleValue) extends AnyVal {
  import SimpleValueOps._

  def toPolyOutput(address: DionAddress): Either[ToOutputFailure, Transaction.PolyOutput] =
    for {
      quantity <-
        Sized
          .max[BigInt, Lengths.`128`.type](simpleValue.quantity.toLong)
          .leftMap(error => ToOutputFailures.InvalidValueQuantity(simpleValue, error))
    } yield Transaction.PolyOutput(address, quantity)

  def toArbitOutput(address: DionAddress): Either[ToOutputFailure, Transaction.ArbitOutput] =
    for {
      quantity <-
        Sized
          .max[BigInt, Lengths.`128`.type](simpleValue.quantity.toLong)
          .leftMap(error => ToOutputFailures.InvalidValueQuantity(simpleValue, error))
    } yield Transaction.ArbitOutput(address, quantity)
}

object SimpleValueOps {
  sealed trait ToOutputFailure

  object ToOutputFailures {
    case class InvalidValueQuantity(value: SimpleValue, inner: Sized.InvalidLength) extends ToOutputFailure
  }

  trait ToSimpleValueOps {
    implicit def simpleValueOpsFromSimpleValue(value: SimpleValue): SimpleValueOps = new SimpleValueOps(value)
  }

  trait Implicits extends ToSimpleValueOps

  object implicits extends Implicits
}
