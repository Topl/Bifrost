package bifrost.contract

import java.security.InvalidParameterException

import bifrost.{BifrostGenerators, ValidGenerators}
import io.circe.{Json, JsonObject}
import io.circe.syntax._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

import scala.util.{Failure, Success, Try}

class ContractMethodSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators{

  property("deliver called by Producer should return a contract with a pending delivery with the delivery amount (if positive), or a Failure (if negative)") {

    forAll(validContractGen.suchThat( c => {
      val status: String = c.storage("status").get.asString.get
      !status.equals("expired") && !status.equals("complete")
    })) {
      c: Contract => {
        forAll(positiveLongGen) {
          quantity: Long =>

            val oldFulfillment = c.storage("currentFulfillment").getOrElse(Map("pendingDelivery" -> List[Json]().asJson).asJson).asObject.get

            if(quantity != 0L) {
              val result: Try[Either[Contract, Json]] = Contract.execute(c, "deliver")(c.Producer)(Map("quantity" -> quantity.asJson).asJsonObject)

              result shouldBe a [Success[Left[Contract, Json]]]

              val newCurrentFulfillmentOpt = result.get.left.get.storage("currentFulfillment")

              newCurrentFulfillmentOpt shouldBe defined

              val newPendingDeliveries = newCurrentFulfillmentOpt.get.asObject.get("pendingDelivery").get.asArray.get
              val oldPendingDeliveries = oldFulfillment("pendingDelivery").get.asArray.get

              newPendingDeliveries.size shouldBe oldPendingDeliveries.size + 1
              newPendingDeliveries(newPendingDeliveries.size - 1).asObject.get("quantity") shouldBe defined
              newPendingDeliveries(newPendingDeliveries.size - 1).asObject.get("quantity").get.asNumber.get.toLong.get shouldBe quantity
            }

            Contract.execute(c, "deliver")(c.Producer)(Map("quantity" -> (-1L * quantity).asJson).asJsonObject) shouldBe a [Failure[InvalidParameterException]]
        }
      }
    }

  }

  property("deliver called when contract status is expired or complete should return Failure") {
    forAll(validContractGen.suchThat( c => {
      val status: String = c.storage("status").get.asString.get
      status.equals("expired") || status.equals("complete")
    })) {
      c: Contract => {
        forAll(positiveLongGen) {
          quantity: Long =>
            Contract.execute(c, "deliver")(c.Producer)(Map("quantity" -> quantity.asJson).asJsonObject) shouldBe a [Failure[IllegalStateException]]
        }
      }
    }
  }

  property("confirmDelivery called by Hub should return a contract with an amount incremented by the confirmed delivery") {
    forAll(validContractGen.suchThat( c => {
      val status: String = c.storage("status").get.asString.get
      !status.equals("expired") && !status.equals("complete")
    })) {
      c: Contract => {
        forAll(positiveLongGen) {
          quantity: Long =>

            val currentFulfillmentJsonObj: JsonObject = c.storage("currentFulfillment").getOrElse(Map("deliveredQuantity" -> 0L.asJson).asJson).asObject.get
            val newFulfillmentJsonObj: JsonObject = currentFulfillmentJsonObj.add(
              "deliveredQuantity",
              (currentFulfillmentJsonObj("deliveredQuantity").getOrElse(0L.asJson).asNumber.get.toLong.get + quantity).asJson
            )
            val expectedNewStorage = c.storage.add("currentFulfillment", newFulfillmentJsonObj.asJson)

            if(quantity != 0L) {
              val result: Try[Either[Contract, Json]] = Contract.execute(c, "deliver")(c.Producer)(Map("quantity" -> quantity.asJson).asJsonObject)
              result shouldBe a [Success[Left[Contract, Json]]]
              result.get.left.get.storage shouldBe expectedNewStorage
            }

            Contract.execute(c, "deliver")(c.Producer)(Map("quantity" -> (-1L * quantity).asJson).asJsonObject) shouldBe a [Failure[InvalidParameterException]]
        }
      }
    }
  }
}