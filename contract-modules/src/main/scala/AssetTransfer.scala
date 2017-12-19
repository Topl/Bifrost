import scala.scalajs.js
import scala.scalajs.js.{Date, JSON}
import scala.scalajs.js.annotation.{JSExport, JSExportStatic, JSExportTopLevel, ScalaJSDefined}

@ScalaJSDefined
@JSExportTopLevel("co.Topl.AssetTransfer")
class AssetTransfer(assetType: String, amount: BigInt) extends BaseModule{
  override def inBreach(): Boolean = ???

  override def isFinished(): Boolean = ???

  override val effectiveDate: BigInt = _
  override val expirationDate: Option[BigInt] = _
  override val initialCapital: BigInt = _

  override def jsonSerializer: JSJsonSerializer[Nothing] = ???

  override def nextPayment(): Option[(BigInt, BigInt)] = ???
}
