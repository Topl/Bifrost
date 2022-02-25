package co.topl.modifier.ops

import cats.implicits._
import co.topl.models.utility.HasLength.instances.bigIntLength
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models.{DionAddress, Transaction}
import co.topl.modifier.box.SimpleValue

import scala.language.implicitConversions

class SimpleValueOps(val simpleValue: SimpleValue) extends AnyVal {
  import SimpleValueOps._

  /**
   * Attempts to convert the [[SimpleValue]] into a [[Transaction.PolyOutput]] with the given [[DionAddress]].
   * @param address the address which will be the recipient of the output
   * @return a [[Transaction.PolyOutput]] if successful, otherwise a [[ToOutputFailure]]
   */
  def toPolyOutput(address: DionAddress): Either[ToOutputFailure, Transaction.PolyOutput] =
    for {
      quantity <-
        Sized
          .max[BigInt, Lengths.`128`.type](simpleValue.quantity.toLong)
          .leftMap(error => ToOutputFailures.InvalidValueQuantity(simpleValue, error))
    } yield Transaction.PolyOutput(address, quantity)

  /**
   * Attempts to convert the [[SimpleValue]] into a [[Transaction.ArbitOutput]] with the given [[DionAddress]].
   * @param address the address which will be the recipient of the output
   * @return a [[Transaction.ArbitOutput]] if successful, otherwise a [[ToOutputFailure]]
   */
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
