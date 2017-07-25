package bifrost.contract.base

import scala.scalajs.js.Date
import scala.scalajs.js.annotation.{JSExportTopLevel, ScalaJSDefined}

/**
  * Created by Matt Kindy on 7/25/2017.
  */

@ScalaJSDefined
@JSExportTopLevel("co.Topl.Loan")
class Loan(val principal: BigInt,
           val accruedInterest: BigInt,
           val paymentInterval: BigInt,
           val interestRate: BigDecimal,
           val interestInterval: BigInt,
           val startingDate: BigInt,
           val totalPaid: BigInt,
           val lastPayment: Option[(BigInt, BigInt)],
           val nextPayment: Option[(BigInt, BigInt)]) extends BaseModule {

  override def inBreach(): Boolean = nextPayment.isDefined && nextPayment.get._1 < new Date().getUTCMilliseconds

  override def isFinished(): Boolean = nextPayment.isEmpty

  def updated(newPrincipal: BigInt,
              newAccruedInterest: BigInt,
              newTotalPaid: BigInt,
              newLastPayment: Option[(BigInt, BigInt)],
              newNextPayment: Option[(BigInt, BigInt)]
             ): Loan =
    new Loan(newPrincipal, newAccruedInterest, paymentInterval, interestRate, interestInterval, startingDate, newTotalPaid, newLastPayment, newNextPayment)

  def makePayment(amount: BigInt): Loan = {

    if(nextPayment.isEmpty) this
    else {
      var newAccruedInterest = accruedInterest
      var newPrincipal = principal

      if(accruedInterest <= amount) {
        newAccruedInterest = 0
        newPrincipal = (principal - (amount - accruedInterest)).max(0)
      } else {
        newAccruedInterest -= amount
      }

      val justPaid: (BigInt, BigInt) = (new Date().getUTCMilliseconds, amount)
      val nextToPay: Option[(BigInt, BigInt)] = if (amount >= nextPayment.get._2) {
        val paymentAmount = (interestRate*(BigDecimal(paymentInterval)/BigDecimal(interestInterval)))*BigDecimal(principal)

        if(paymentAmount > 0)
          Some((new Date().getUTCMilliseconds + paymentInterval, paymentAmount.toBigInt))
        else
          None

      } else nextPayment.map(n => n._1 -> (n._2 - amount))

      updated(newPrincipal, newAccruedInterest, totalPaid + amount, Some(justPaid), nextToPay)
    }
  }

  override val initialCapital: BigInt = principal
  override val effectiveDate: BigInt = startingDate
  override val expirationDate = None
}
