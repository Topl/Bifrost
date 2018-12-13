package bifrost.contract

import java.time.Instant

import bifrost.transaction.AssetCreation
import bifrost.{BifrostGenerators, ValidGenerators}
import io.circe.JsonObject
import io.circe.syntax._
import org.graalvm.polyglot.management.ExecutionListener
import org.graalvm.polyglot.{Context, Value}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class ContractMethodSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Can call a function from a contract") {
    forAll(contractGen) {
      c: Contract => {
        val party = propositionGen.sample.get
        val params = JsonObject.fromMap(
          Map("newStatus" -> stringGen.sample.get.asJson))

        println(s">>>>>>>>> stringGen: ${params}")
        val result = Contract.execute(c, "changeStatus")(party)(params)
        println(s"test result: $result")
        assert(result.isSuccess)
      }
    }
  }

  property("Can call createAsset protocol level function from a contract") {
    forAll(contractGen) {
      c: Contract => {
        val party = propositionGen.sample.get
        val params = JsonObject.fromMap(
          Map(
            "publicKey" -> stringGen.sample.get.asJson,
            "asset" -> stringGen.sample.get.asJson,
            "amount" -> positiveTinyIntGen.sample.get.asJson))

        val result = Contract.execute(c, "createAsset")(party)(params)
        println(s"test result: $result")
        assert(result.isSuccess)
      }
    }
  }

  property("Can call assetTransfer protocol level function from a contract") {
    forAll(contractGen) {
      c: Contract => {
        val party = propositionGen.sample.get
        val params = JsonObject.fromMap(
          Map(
            "publicKey" -> stringGen.sample.get.asJson,
            "asset" -> stringGen.sample.get.asJson,
            "amount" -> positiveTinyIntGen.sample.get.asJson,
            "data" -> stringGen.sample.get.asJson))

        val result = Contract.execute(c, "transferAssets")(party)(params)
        println(result)
        assert(result.isSuccess)
      }
    }
  }

    /*val name: String = "assetCreation"
    val assetCode: String = "Wheat"
    val effectiveTimestamp = Instant.now.toEpochMilli
    val expirationTimestamp = Instant.now.toEpochMilli + 10000L
    val contract: String =
      s"""
         |this.$name = function(){
         |    this.contractEffectiveTime = $effectiveTimestamp;
         |    this.contractExpirationTime = $expirationTimestamp;
         |    this.status = "initialized"
         |    this.assetCode = "$assetCode"
         |    this.initialCapital = "0";
         |
         |    this.changeStatus = function(newStatus) {
         |      this.status = newStatus;
         |      return this;
         |    }
         |
         |    this.newAsset = function(asset, amount) {
         |      createAsset(asset, amount);
         |      return this;
         |    }
         |}
         |
         |this.$name.fromJSON = function(str) {
         |    return new $name();
         |}
         |
         |this.$name.toJSON = function(o) {
         |    return JSON.stringify(o);
         |}
     """.stripMargin

    var state = "{}".asJson

    val polyglot: Context = Context.create()

    val protocolStateListener: Value = polyglot.eval("js",
      s"""
         |function protocolStateListener() {
         |
       """.stripMargin)

    val createAsset: Value = polyglot.eval("js",
      s"""
         |async function createAsset() {
         |    await
       """.stripMargin)



    val party = propositionGen.sample.get
    val params = JsonObject.fromMap(Map("asset" -> stringGen.sample.get.asJson, "amount" -> positiveTinyIntGen.sample.get.asJson))

    val result = Contract.execute(contract, "createAsset")(party)(params)
    println(s">>>>>>>> Result: $result")*/
}