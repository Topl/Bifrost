import scala.scalajs.js
import scala.scalajs.js.annotation.ScalaJSDefined
import scala.util.Try

/**
  * Created by Matt Kindy on 7/24/2017.
  */
@ScalaJSDefined
abstract class BaseModule[M] extends js.Object {

  def jsonSerializer: JSJsonSerializer[M]

  /* This should return true iff the program is in breach */
  def inBreach(): Boolean

  /* Returns the timestamp and amount for the next payment due */
  def nextPayment(): Option[(BigInt, BigInt)]

  def isFinished(): Boolean

  val initialCapital: BigInt
  val expirationDate: Option[BigInt]
  val effectiveDate: BigInt
}

trait JSJsonSerializer[T] {
  def toJSON(obj: T): String
  def fromJSON(json: String): T

  def apply(args: js.Object): T
}