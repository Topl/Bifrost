package bifrost.contract

import java.time.Instant

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
  with ValidGenerators {

  property("deliver called by Producer should return a contract with a pending delivery with the delivery amount (if positive), or a Failure (if negative)") {

    forAll(validContractGen.suchThat( c => {
      val status: String = c.storage("status").get.asString.get
      !status.equals("expired") && !status.equals("complete")
    })) {
      c: Contract => {
        forAll(Gen.choose(0, Long.MaxValue)) {
          quantity: Long =>

            val oldFulfillment = c.storage("currentFulfillment").getOrElse(Map("pendingDeliveries" -> List[Json]().asJson).asJson).asObject.get

            if(quantity != 0L) {
              val result: Try[Either[Contract, Json]] = Contract.execute(c, "deliver")(c.Producer)(Map("quantity" -> quantity.asJson).asJsonObject)

              result shouldBe a[Success[_]]
              result.get shouldBe a[Left[_,_]]
              result.get.left.get shouldBe a[Contract]

              val newCurrentFulfillmentOpt = result.get.left.get.storage("currentFulfillment")

              newCurrentFulfillmentOpt shouldBe defined

              val newPendingDeliveries = newCurrentFulfillmentOpt.get.asObject.get("pendingDeliveries").get.asArray.get
              val oldPendingDeliveries = oldFulfillment("pendingDeliveries").get.asArray.get

              newPendingDeliveries.size shouldBe oldPendingDeliveries.size + 1
              newPendingDeliveries(newPendingDeliveries.size - 1).asObject.get("quantity") shouldBe defined
              newPendingDeliveries(newPendingDeliveries.size - 1).asObject.get("quantity").get.asNumber.get.toLong.get shouldBe quantity
            }

            val result = Contract.execute(c, "deliver")(c.Producer)(Map("quantity" -> (-1L * quantity).asJson).asJsonObject)

            result shouldBe a[Failure[_]]
            result.failed.get shouldBe a[IllegalArgumentException]
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

            val result = Contract.execute(c, "deliver")(c.Producer)(Map("quantity" -> quantity.asJson).asJsonObject)

            result shouldBe a[Failure[_]]
            result.failed.get shouldBe a[IllegalStateException]
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

              val result = Contract.execute(contract, "confirmDelivery")(contract.Hub)(
                Map(
                  "deliveryId" -> deliveryToEndorse("id").get
                ).asJsonObject
              )

              result shouldBe a[Success[_]]
              result.get shouldBe a[Left[_,_]]
              result.get.left.get shouldBe a[Contract]

              contract = result.get.left.get

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
        forAll(positiveTinyIntGen.suchThat(_ > 0)) {
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
                val result = Contract.execute(contract, "confirmDelivery")(contract.Hub)(
                  Map(
                    "deliveryId" -> fakeId.asJson
                  ).asJsonObject
                )
                result shouldBe a[Failure[_]]
                result.failed.get shouldBe a[NoSuchElementException]
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

              val expiredResult = Contract.execute(expiredContract, "confirmDelivery")(contract.Hub)(
                Map(
                "deliveryId" -> deliveryToEndorse("id").get
                ).asJsonObject
              )

              expiredResult shouldBe a[Failure[_]]
              expiredResult.failed.get shouldBe a[IllegalStateException]

              val completedResult = Contract.execute(completedContract, "confirmDelivery")(contract.Hub)(
                Map(
                  "deliveryId" -> deliveryToEndorse("id").get
                ).asJsonObject
              )

              completedResult shouldBe a[Failure[_]]
              completedResult.failed.get shouldBe a[IllegalStateException]

            })
          }
        }
      }
    }
  }

  property("currentStatus should successfully return the current status of the contract as Json") {
    forAll(validContractGen) {
      c: Contract =>
        val result = Contract.execute(c, "currentStatus")(Gen.oneOf(Seq(c.Producer, c.Hub, c.Investor)).sample.get)(JsonObject.empty)

        result shouldBe a[Success[_]]
        result.get shouldBe a[Left[_,_]]
        result.get.left.get shouldBe a[Json]
        result.get.left.get shouldBe c.storage("status").get
    }
  }

  property("checkExpiration should update a contract with expirationTimestamp that has passed if not yet expired") {

    val lateAgreementGen = for {
      terms <- agreementTermsGen
      diff <- positiveLongGen
    } yield Agreement(terms, Instant.now.toEpochMilli - diff - 5000, Instant.now.toEpochMilli - diff)

    val expirableContractGen = for {
      producer <- propositionGen
      investor <- propositionGen
      hub <- propositionGen
      storage <- jsonGen()
      status <- Gen.oneOf(validStatuses.filterNot(s => s.equals("expired") || s.equals("complete")))
      agreement <- lateAgreementGen.map(_.json)
      id <- genBytesList(FastCryptographicHash.DigestSize)
    } yield Contract(Map(
      "producer" -> Base58.encode(producer.pubKeyBytes).asJson,
      "investor" -> Base58.encode(investor.pubKeyBytes).asJson,
      "hub" -> Base58.encode(hub.pubKeyBytes).asJson,
      "storage" -> Map("status" -> status.asJson, "other" -> storage).asJson,
      "agreement" -> agreement
    ).asJson, id)

    forAll(expirableContractGen) {
      c: Contract =>
        val result = Contract.execute(c, "checkExpiration")(Gen.oneOf(Seq(c.Producer, c.Hub, c.Investor)).sample.get)(JsonObject.empty)
        result shouldBe a[Success[_]]
        result.get shouldBe a[Left[_,_]]
        result.get.left.get shouldBe a[Contract]

        result.get.left.get.storage("status").get shouldBe "expired".asJson
    }
  }

  property("checkExpiration should not change a contract that is already expired or which hasn't passed the expirationTimestamp") {

    forAll(validContractGen.suchThat(c => {
      val status = c.storage("status").get
      val cannotExpire = status.equals("expired".asJson) || status.equals("complete".asJson)
      val notPastExpiration = c.agreement("expirationTimestamp").get.asNumber.get.toLong.get > Instant.now.toEpochMilli + 5000L

      notPastExpiration || cannotExpire
    })) {
      c: Contract =>
        val result = Contract.execute(c, "checkExpiration")(Gen.oneOf(Seq(c.Producer, c.Hub, c.Investor)).sample.get)(JsonObject.empty)
        result shouldBe a[Success[_]]
        result.get shouldBe a[Left[_,_]]
        result.get.left.get shouldBe a[Contract]

        result.get.left.get.storage("status").get shouldBe c.storage("status").get
    }
  }

  property("endorseCompletion should create an endorsement entry with a hash of the contract state when the contract is not expired or complete") {

    val deliveryGen = for {
      quantity <- positiveLongGen
    } yield {
      Map(
        "quantity" -> quantity.asJson,
        "timestamp" -> Instant.now.toEpochMilli.asJson
      )
    }

    val pendingDeliveriesJsonGen = for {
      length <- positiveTinyIntGen
    } yield {
      var deliveriesSoFar = List[Json]()

      (0 until length) foreach {
        _ => {
          val thisDelivery = deliveryGen.sample.get
          val pdId: String = Base58.encode(
            FastCryptographicHash(
              (deliveriesSoFar :+ thisDelivery.asJson).asJson.noSpaces.getBytes
            )
          )

          deliveriesSoFar = deliveriesSoFar :+
            Map(
              "quantity" -> thisDelivery.get("quantity").asJson,
              "timestamp" -> thisDelivery.get("timestamp").asJson,
              "id" -> pdId.asJson
            ).asJson

        }
      }

      deliveriesSoFar.asJson
    }

    val endorseableContractGen = for {
      producer <- propositionGen
      investor <- propositionGen
      hub <- propositionGen
      storage <- jsonGen()
      status <- Gen.oneOf(validStatuses.filterNot(s => s.equals("expired") || s.equals("complete")))
      agreement <- validAgreementGen.map(_.json)
      id <- genBytesList(FastCryptographicHash.DigestSize)
      pendingDeliveriesJson <- pendingDeliveriesJsonGen
      deliveredQuantity <- positiveLongGen
    } yield Contract(Map(
      "producer" -> Base58.encode(producer.pubKeyBytes).asJson,
      "investor" -> Base58.encode(investor.pubKeyBytes).asJson,
      "hub" -> Base58.encode(hub.pubKeyBytes).asJson,
      "storage" -> Map(
        "status" -> status.asJson,
        "currentFulfillment" -> Map(
          "pendingDeliveries" -> pendingDeliveriesJson,
          "deliveredQuantity" -> deliveredQuantity.asJson
        ).asJson,
        "other" -> storage
      ).asJson,
      "agreement" -> agreement
    ).asJson, id)

    forAll(endorseableContractGen) {
      c: Contract => {

        forAll(positiveTinyIntGen) {
          i: Int =>

            var contract = c

            (0 until i) foreach { _ =>

              val endorser = Gen.oneOf(Seq(contract.Producer, contract.Hub, contract.Investor)).sample.get



              /* Introduce some possible variance in currentFulfillment by creating pending deliveries and/or confirming */
              if(Gen.oneOf(Seq(false, true)).sample.get) {
                contract = Contract.execute(contract, "deliver")(contract.Producer)(Map("quantity" -> positiveLongGen.sample.get.asJson).asJsonObject).get.left.get
              }

              val fulfillment = contract.storage("currentFulfillment").get
              val pendingDeliveries = fulfillment.asObject.get("pendingDeliveries").get.asArray.get

              if(Gen.oneOf(Seq(false, true)).sample.get && pendingDeliveries.nonEmpty) {

                val deliveryToEndorse: JsonObject = Gen.oneOf(pendingDeliveries).sample.get.asObject.get

                contract = Contract.execute(contract, "confirmDelivery")(contract.Hub)(
                  Map(
                    "deliveryId" -> deliveryToEndorse("id").get
                  ).asJsonObject
                ).get.left.get
              }



              val result = Contract.execute(contract, "endorseCompletion")(endorser)(JsonObject.empty)
              result shouldBe a[Success[_]]
              result.get shouldBe a[Left[_, _]]
              result.get.left.get shouldBe a[Contract]

              val newEndorsements = result.get.left.get.storage("endorsements").get.asObject.get
              val oldEndorsements = contract.storage("endorsements").getOrElse(Map[String, Json]().asJson).asObject.get

              val oldFulfillment = contract.storage("currentFulfillment").getOrElse(Map[String, Json]().asJson)

              contract = result.get.left.get

              require(newEndorsements.fields.length >= oldEndorsements.fields.length)
              newEndorsements.fields.exists(_.equals(Base58.encode(endorser.pubKeyBytes))) shouldBe true
              newEndorsements(Base58.encode(endorser.pubKeyBytes)).get shouldBe Base58.encode(FastCryptographicHash(oldFulfillment.noSpaces.getBytes)).asJson
            }
        }
      }
    }
  }

  property("endorseCompletion should not change a contract that is already expired or completed") {

    val deliveryGen = for {
      quantity <- positiveLongGen
    } yield {
      Map(
        "quantity" -> quantity.asJson,
        "timestamp" -> Instant.now.toEpochMilli.asJson
      )
    }

    val pendingDeliveriesJsonGen = for {
      length <- positiveTinyIntGen
    } yield {
      var deliveriesSoFar = List[Json]()

      (0 until length) foreach {
        _ => {
          val thisDelivery = deliveryGen.sample.get
          val pdId: String = Base58.encode(
            FastCryptographicHash(
              (deliveriesSoFar :+ thisDelivery.asJson).asJson.noSpaces.getBytes
            )
          )

          deliveriesSoFar = deliveriesSoFar :+
            Map(
              "quantity" -> thisDelivery.get("quantity").asJson,
              "timestamp" -> thisDelivery.get("timestamp").asJson,
              "id" -> pdId.asJson
            ).asJson

        }
      }

      deliveriesSoFar.asJson
    }

    val nonEndorseableContractGen = for {
      producer <- propositionGen
      investor <- propositionGen
      hub <- propositionGen
      storage <- jsonGen()
      status <- Gen.oneOf(validStatuses.filter(s => s.equals("expired") || s.equals("complete")))
      agreement <- validAgreementGen.map(_.json)
      id <- genBytesList(FastCryptographicHash.DigestSize)
      pendingDeliveriesJson <- pendingDeliveriesJsonGen
      deliveredQuantity <- positiveLongGen
    } yield Contract(Map(
      "producer" -> Base58.encode(producer.pubKeyBytes).asJson,
      "investor" -> Base58.encode(investor.pubKeyBytes).asJson,
      "hub" -> Base58.encode(hub.pubKeyBytes).asJson,
      "storage" -> Map(
        "status" -> status.asJson,
        "currentFulfillment" -> Map(
          "pendingDeliveries" -> pendingDeliveriesJson,
          "deliveredQuantity" -> deliveredQuantity.asJson
        ).asJson,
        "endorsements" -> Map(
          Base58.encode(Gen.oneOf(Seq(producer, investor, hub)).sample.get.pubKeyBytes) -> Base58.encode(
            FastCryptographicHash(
              Map(
                "pendingDeliveries" -> pendingDeliveriesJson,
                "deliveredQuantity" -> deliveredQuantity.asJson
              ).asJson.noSpaces.getBytes
            )
          )
        ).asJson,
        "other" -> storage
      ).asJson,
      "agreement" -> agreement
    ).asJson, id)

    forAll(nonEndorseableContractGen) {
      c: Contract =>
        val result = Contract.execute(c, "endorseCompletion")(Gen.oneOf(Seq(c.Producer, c.Hub, c.Investor)).sample.get)(JsonObject.empty)
        result shouldBe a[Failure[_]]
        result.failed.get shouldBe a[IllegalStateException]
    }
  }
}