import scala.scalajs.js
import scala.scalajs.js.annotation.ScalaJSDefined
import scala.util.Try

/**
  * Created by Matt Kindy on 7/24/2017.
  */
@ScalaJSDefined
abstract class BaseModule extends js.Object {

  type M <: this.type

  val jsonSerializer: JSJsonSerializer[M]

  /* This should return true iff the contract is in breach */
  def inBreach(): Boolean

  /* Returns the timestamp and amount for the next payment due */
  def nextPayment(): Option[(BigInt, BigInt)]

  def isFinished(): Boolean

  val initialCapital: BigInt
  val expirationDate: Option[BigInt]
  val effectiveDate: BigInt
}

trait JSJsonSerializer[M] {
  def toJSON(obj: M): String
  def fromJSON(json: String): Try[M]
}