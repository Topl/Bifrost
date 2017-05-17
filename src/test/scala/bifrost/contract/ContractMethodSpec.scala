package bifrost.contract

import java.security.InvalidParameterException

import bifrost.{BifrostGenerators, ValidGenerators}
import io.circe.{Json, JsonObject}
import io.circe.syntax._
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.crypto.encode.Base58

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

            val oldFulfillment = c.storage("currentFulfillment").getOrElse(Map("pendingDeliveries" -> List[Json]().asJson).asJson).asObject.get

            if(quantity != 0L) {
              val result: Try[Either[Contract, Json]] = Contract.execute(c, "deliver")(c.Producer)(Map("quantity" -> quantity.asJson).asJsonObject)

              result shouldBe a[Success[Left[Contract, Json]]]

              val newCurrentFulfillmentOpt = result.get.left.get.storage("currentFulfillment")

              newCurrentFulfillmentOpt shouldBe defined

              val newPendingDeliveries = newCurrentFulfillmentOpt.get.asObject.get("pendingDeliveries").get.asArray.get
              val oldPendingDeliveries = oldFulfillment("pendingDeliveries").get.asArray.get

              newPendingDeliveries.size shouldBe oldPendingDeliveries.size + 1
              newPendingDeliveries(newPendingDeliveries.size - 1).asObject.get("quantity") shouldBe defined
              newPendingDeliveries(newPendingDeliveries.size - 1).asObject.get("quantity").get.asNumber.get.toLong.get shouldBe quantity
            }

            Contract.execute(c, "deliver")(c.Producer)(Map("quantity" -> (-1L * quantity).asJson).asJsonObject) shouldBe a[Failure[InvalidParameterException]]
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
            Contract.execute(c, "deliver")(c.Producer)(Map("quantity" -> quantity.asJson).asJsonObject) shouldBe a[Failure[IllegalStateException]]
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
        forAll(positiveTinyIntGen) {
          i: Int => {
            var contract = c

            contract.storage("currentFulfillment").getOrElse(
              Map(
                "pendingDeliveries" -> List[Json]().asJson
              ).asJson
            ).asObject.get("deliveredQuantity").getOrElse(0L.asJson) shouldBe 0L.asJson

            /* Add a variable number of pending deliveries */
            (0 until i).foreach(_ => {
              val quantity = positiveLongGen.sample.get
              contract = Contract.execute(contract, "deliver")(contract.Producer)(
                Map(
                  "quantity" -> quantity.asJson
                ).asJsonObject
              ).get.left.get
            })

            var fulfillment = contract.storage("currentFulfillment").get
            var pendingDeliveries = fulfillment.asObject.get("pendingDeliveries").get.asArray.get

            pendingDeliveries.size shouldBe i


            /* Resolve the pending deliveries */
            (0 until i).foreach(_ => {

              val fulfillment = contract.storage("currentFulfillment").get
              val pendingDeliveries = fulfillment.asObject.get("pendingDeliveries").get.asArray.get

              require(pendingDeliveries.nonEmpty, "No pending deliveries when pending deliveries were expected.")

              val deliveryToEndorse: JsonObject = Gen.oneOf(pendingDeliveries).sample.get.asObject.get

              contract = Contract.execute(contract, "confirmDelivery")(contract.Hub)(
                Map(
                  "deliveryId" -> deliveryToEndorse("id").get
                ).asJsonObject
              ).get.left.get

              val newFulfillment = contract.storage("currentFulfillment").get
              val newPendingDeliveries = newFulfillment.asObject.get("pendingDeliveries").get.asArray.get

              val newDeliveredQuantity = newFulfillment.asObject.get("deliveredQuantity").getOrElse(0L.asJson).asNumber.get.toLong.get
              val oldDeliveredQuantity = fulfillment.asObject.get("deliveredQuantity").getOrElse(0L.asJson).asNumber.get.toLong.get

              val endorsedQuantity = deliveryToEndorse("quantity").get.asNumber.get.toLong.get

              newDeliveredQuantity shouldBe (oldDeliveredQuantity + endorsedQuantity)
              newPendingDeliveries.size shouldBe pendingDeliveries.size - 1
              newPendingDeliveries.exists(_.asObject.get("id").get.equals(deliveryToEndorse("id").get)) shouldBe false

            })

            fulfillment = contract.storage("currentFulfillment").get
            pendingDeliveries = fulfillment.asObject.get("pendingDeliveries").get.asArray.get

            pendingDeliveries shouldBe empty
          }
        }
      }
    }
  }

  property("confirmDelivery called for an id that doesn't exist should return Failure") {
    forAll(validContractGen.suchThat( c => {
      val status: String = c.storage("status").get.asString.get
      !status.equals("expired") && !status.equals("complete")
    })) {
      c: Contract => {
        forAll(positiveTinyIntGen) {
          i: Int => {
            var contract = c

            contract.storage("currentFulfillment").getOrElse(
              Map(
                "pendingDeliveries" -> List[Json]().asJson
              ).asJson
            ).asObject.get("deliveredQuantity").getOrElse(0L.asJson) shouldBe 0L.asJson

            /* Add a variable number of pending deliveries */
            (0 until i).foreach(_ => {
              val quantity = positiveLongGen.sample.get
              contract = Contract.execute(contract, "deliver")(contract.Producer)(Map("quantity" -> quantity.asJson).asJsonObject).get.left.get
            })

            val fulfillment = contract.storage("currentFulfillment").get
            val pendingDeliveries = fulfillment.asObject.get("pendingDeliveries").get.asArray.get

            pendingDeliveries.size shouldBe i

            forAll(nonEmptyBytesGen) {
              b: Array[Byte] => {
                val fakeId: String = Base58.encode(FastCryptographicHash(b))
                Contract.execute(contract, "confirmDelivery")(contract.Hub)(
                  Map(
                    "deliveryId" -> fakeId.asJson
                  ).asJsonObject
                ) shouldBe a[Failure[NoSuchElementException]]
              }
            }

          }
        }
      }
    }
  }

  property("confirmDelivery called when contract status is expired or complete should return Failure") {
    forAll(validContractGen.suchThat( c => {
      val status: String = c.storage("status").get.asString.get
      !status.equals("expired") && !status.equals("complete")
    })) {
      c: Contract => {
        forAll(positiveTinyIntGen) {
          i: Int => {
            var contract = c

            contract.storage("currentFulfillment").getOrElse(
              Map(
                "pendingDeliveries" -> List[Json]().asJson
              ).asJson
            ).asObject.get("deliveredQuantity").getOrElse(0L.asJson) shouldBe 0L.asJson

            /* Add a variable number of pending deliveries */
            (0 until i).foreach(_ => {
              val quantity = positiveLongGen.sample.get
              contract = Contract.execute(contract, "deliver")(contract.Producer)(Map("quantity" -> quantity.asJson).asJsonObject).get.left.get
            })

            val fulfillment = contract.storage("currentFulfillment").get
            val pendingDeliveries = fulfillment.asObject.get("pendingDeliveries").get.asArray.get

            pendingDeliveries.size shouldBe i

            val expiredContract = new Contract(
              contract.Producer, contract.Hub, contract.Investor,
              contract.storage.add("status", "expired".asJson),
              contract.agreement, contract.id
            )

            val completedContract = new Contract(
              contract.Producer, contract.Hub, contract.Investor,
              contract.storage.add("status", "complete".asJson),
              contract.agreement, contract.id
            )

            /* Resolve the pending deliveries */
            (0 until i).foreach(_ => {

              val fulfillment = contract.storage("currentFulfillment").get
              val pendingDeliveries = fulfillment.asObject.get("pendingDeliveries").get.asArray.get

              require(pendingDeliveries.nonEmpty, "No pending deliveries when pending deliveries were expected.")

              val deliveryToEndorse: JsonObject = Gen.oneOf(pendingDeliveries).sample.get.asObject.get

              Contract.execute(expiredContract, "confirmDelivery")(contract.Hub)(
                Map(
                "deliveryId" -> deliveryToEndorse("id").get
                ).asJsonObject
              ) shouldBe a[Failure[IllegalStateException]]

              Contract.execute(completedContract, "confirmDelivery")(contract.Hub)(
                Map(
                  "deliveryId" -> deliveryToEndorse("id").get
                ).asJsonObject
              ) shouldBe a[Failure[IllegalStateException]]

            })
          }
        }
      }
    }
  }
}