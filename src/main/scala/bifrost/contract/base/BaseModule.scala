package bifrost.contract.base

import scala.scalajs.js
import scala.scalajs.js.annotation.ScalaJSDefined

/**
  * Created by Matt Kindy on 7/24/2017.
  */
@ScalaJSDefined
abstract class BaseModule extends js.Object {

  /* This should return true iff the contract is in breach */
  def inBreach(): Boolean

  /* Returns the timestamp and amount for the next payment due */
  def nextPayment(): Option[(BigInt, BigInt)]

  def isFinished(): Boolean

  val initialCapital: BigInt
  val expirationDate: Option[BigInt]
  val effectiveDate: BigInt
}
