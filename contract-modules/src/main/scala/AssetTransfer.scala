import scala.scalajs.js
import scala.scalajs.js.{Date, JSON}
import scala.scalajs.js.annotation.{JSExport, JSExportStatic, JSExportTopLevel, ScalaJSDefined}

@ScalaJSDefined
@JSExportTopLevel("co.Topl.AssetTransfer")
class AssetTransfer(assetType: String, amount: BigInt) extends BaseModule[AssetTransfer] {
  override def inBreach(): Boolean = false

  override def isFinished(): Boolean = nextPayment().isEmpty

  override val effectiveDate: BigInt = _
  override val expirationDate: Option[BigInt] = None
  override val initialCapital: BigInt = _

  override def jsonSerializer: JSJsonSerializer[AssetTransfer] = AssetTransfer

  override def nextPayment(): Option[(BigInt, BigInt)] = ???

  object AssetTransfer extends JSJsonSerializer[AssetTransfer] {

    @JSExportStatic
    def toJSON(l: AssetTransfer): String = scalajs.js.JSON.stringify(l)

    @JSExportStatic
    def fromJSON(json: String): AssetTransfer = {
      val parsed = scalajs.js.JSON.parse(json)
      new AssetTransfer(
        parsed.selectDynamic("assetType").asInstanceOf[String],
        BigInt(parsed.selectDynamic("amount").asInstanceOf[String])
      )
    }
  }

  @JSExportStatic("construct")
  def apply (args: js.Object): AssetTransfer = fromJSON(JSON.stringify(args))
}