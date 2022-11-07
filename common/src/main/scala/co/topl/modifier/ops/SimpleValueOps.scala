package co.topl.modifier.ops

import cats.implicits._
import co.topl.models.utility.HasLength.instances.bigIntLength
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models.{Box => TetraBox, FullAddress, Transaction}
import co.topl.modifier.box.SimpleValue

import scala.language.implicitConversions

class SimpleValueOps(private val simpleValue: SimpleValue) extends AnyVal {
  import SimpleValueOps._

  /**
   * Attempts to convert the [[SimpleValue]] into a [[Transaction.Output]] with the given [[SpendingAddress]].
   *
   * @param address the address which will be the recipient of the output
   * @return a [[Transaction.Output]] if successful, otherwise a [[ToOutputFailure]]
   */
  def toPolyOutput(address: FullAddress): Either[ToOutputFailure, Transaction.Output] =
    for {
      quantity <-
        Sized
          .max[BigInt, Lengths.`128`.type](BigInt(simpleValue.quantity.toByteArray))
          // this should never fail to be a valid quantity, but just in case
          .leftMap(error => ToOutputFailures.InvalidValueQuantity(simpleValue, error))
      value = TetraBox.Values.Poly(quantity)
    } yield Transaction.Output(address, value, minting = false)

  /**
   * Attempts to convert the [[SimpleValue]] into a [[Transaction.Output]] with the given [[SpendingAddress]].
   *
   * @param address the address which will be the recipient of the output
   * @return a [[Transaction.Output]] if successful, otherwise a [[ToOutputFailure]]
   */
  def toArbitOutput(address: FullAddress): Either[ToOutputFailure, Transaction.Output] =
    for {
      quantity <-
        Sized
          .max[BigInt, Lengths.`128`.type](BigInt(simpleValue.quantity.toByteArray))
          // this should never fail to be a valid quantity, but just in case
          .leftMap(error => ToOutputFailures.InvalidValueQuantity(simpleValue, error))
    } yield Transaction.Output(address, TetraBox.Values.Arbit(quantity), minting = false)
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
