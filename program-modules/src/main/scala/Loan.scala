import scala.scalajs.js
import scala.scalajs.js.{Date, JSON}
import scala.scalajs.js.annotation.{JSExport, JSExportStatic, JSExportTopLevel, ScalaJSDefined}

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
           val nextPayment: Option[(BigInt, BigInt)]) extends BaseModule[Loan] {

  override def inBreach(): Boolean = nextPayment.isDefined && nextPayment.get._1 < new Date().getUTCMilliseconds

  override def isFinished(): Boolean = nextPayment.isEmpty

  def updated(newPrincipal: BigInt,
              newAccruedInterest: BigInt,
              newTotalPaid: BigInt,
              newLastPayment: Option[(BigInt, BigInt)],
              newNextPayment: Option[(BigInt, BigInt)]
             ): Loan =
    new Loan(newPrincipal, newAccruedInterest, paymentInterval, interestRate, interestInterval, startingDate, newTotalPaid, newLastPayment, newNextPayment)

  def makePayment(amt: String): Loan = {

    val amount = BigInt(amt)
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

      val justPaid: (BigInt, BigInt) = (new Date().getUTCMilliseconds, (accruedInterest + principal).min(amount))
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

  override val jsonSerializer: JSJsonSerializer[Loan] = Loan
  override val initialCapital: BigInt = principal
  override val effectiveDate: BigInt = startingDate
  override val expirationDate = None
}

object Loan extends JSJsonSerializer[Loan] {

  @JSExportStatic
  def toJSON(l: Loan): String = scalajs.js.JSON.stringify(l)

  @JSExportStatic
  def fromJSON(json: String): Loan = {
    val parsed = scalajs.js.JSON.parse(json)
    new Loan(
      BigInt(parsed.selectDynamic("principal").asInstanceOf[String]),
      BigInt(parsed.selectDynamic("accruedInterest").asInstanceOf[String]),
      BigInt(parsed.selectDynamic("paymentInterval").asInstanceOf[String]),
      BigDecimal(parsed.selectDynamic("interestRate").asInstanceOf[String]),
      BigInt(parsed.selectDynamic("interestInterval").asInstanceOf[String]),
      BigInt(parsed.selectDynamic("startingDate").asInstanceOf[String]),
      BigInt(parsed.selectDynamic("totalPaid").asInstanceOf[String]),
      parsed.selectDynamic("lastPayment")
        .asInstanceOf[scalajs.js.UndefOr[scalajs.js.Array[String]]]
        .toOption.map(o => BigInt(o(0)) -> BigInt(o(1))),
      parsed.selectDynamic("nextPayment")
        .asInstanceOf[scalajs.js.UndefOr[scalajs.js.Array[String]]]
        .toOption.map(o => BigInt(o(0)) -> BigInt(o(1)))
    )
  }

  @JSExportStatic("construct")
  def apply(args: js.Object): Loan = fromJSON(JSON.stringify(args))
}
