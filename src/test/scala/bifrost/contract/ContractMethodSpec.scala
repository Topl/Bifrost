package bifrost.contract

import java.security.InvalidParameterException

import bifrost.{BifrostGenerators, ValidGenerators}
import io.circe.JsonObject
import io.circe.syntax._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

import scala.util.Failure

class ContractMethodSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators{

  property("deliver called by Producer should return a contract with an amount incremented by the delivery amount (if positive), or a Failure (if negative)") {

    forAll(contractGen) {
      c: Contract => {
        forAll(positiveLongGen) {
          quantity: Long =>

            val currentFulfillmentJsonObj: JsonObject = c.storage("currentFulfillment").getOrElse(Map("deliveredQuantity" -> 0L.asJson).asJson).asObject.get
            val newFulfillmentJsonObj: JsonObject = currentFulfillmentJsonObj.add(
              "deliveredQuantity",
              (currentFulfillmentJsonObj("deliveredQuantity").getOrElse(0L.asJson).asNumber.get.toLong.get + quantity).asJson
            )
            val expectedNewStorage = c.storage.add("currentFulfillment", newFulfillmentJsonObj.asJson)

            Contract.execute(c, "deliver")(c.Producer)(Map("quantity" -> quantity.asJson).asJsonObject).get.left.get.storage shouldBe expectedNewStorage

            if(quantity != 0L) {
              Contract.execute(c, "deliver")(c.Producer)(Map("quantity" -> (-1L * quantity).asJson).asJsonObject) shouldBe a [Failure[InvalidParameterException]]
            }
        }
      }
    }

  }
}