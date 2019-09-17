package bifrost.program

import java.net.URLDecoder
import java.nio.file.{Path, Paths}

import bifrost.{BifrostGenerators, ValidGenerators}
import io.circe.JsonObject
import io.circe.syntax._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}


class ProgramPreprocessorSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  val args: JsonObject = Map(
    "principal" -> "0".asJson,
    "accruedInterest" -> "0".asJson,
    "paymentInterval" -> "0".asJson,
    "interestRate" -> "0".asJson,
    "interestInterval" -> "0".asJson,
    "startingDate" -> "0".asJson,
    "totalPaid" -> "0".asJson,
    "lastPayment" -> Array("0", "0").map(_.asJson).asJson,
    "nextPayment" -> Array("0", "0").map(_.asJson).asJson
  ).asJsonObject

  val name = "test"

  val script =
    s"""
       |this.test = function() {
       |  var x = 1;
       |  var y = "test";
       |
         |  this.add = function(a,b) {
       |    return a + b;
       |  }
       |
         |  this.sub = function(a,b) {
       |    return a - b;
       |  }
       |
         |  this.program = function() {
       |    this.status = "initialized";
       |
         |    this.newStatus = function(input) {
       |      status = input;
       |      return this;
       |    }
       |
         |    this.changeStatus = function(input) {
       |      status = input;
       |      return this;
       |    }
       |  }
       |}
       |
         |
         |this.$name.fromJSON = function(str) {
       |    return new $name();
       |}
       |
         |this.$name.toJSON = function(o) {
       |    return JSON.stringify(o);
       |}
       """.stripMargin

  val program =
    s"""
       |this.$name = function() {
       |    this.status = "initialized";
       |    this.assetCode = "Wheat";
       |    this.initialCapital = "0";
       |
       |  this.changeStatus = function(newStatus) {
       |      this.status = newStatus;
       |      return this;
       |    };
       |};
       |
       |this.$name.fromJSON = function(str) {
       |    return new $name();
       |};
       |
       |this.$name.toJSON = function(o) {
       |    return JSON.stringify(o);
       |};
       """.stripMargin

  val simpleTest =
    s"""
       |var test = "test";
     """.stripMargin

  /*val filePath = URLDecoder.decode(this.getClass.getResource("/program-modules-fastopt.json").getPath, "UTF-8")
  val osAppropriatePath: Path = Paths.get(
    if (System.getProperty("os.name").contains("indow")) filePath.substring(1) else filePath
  )*/

  /*property("Json encoding and decoding should work") {
    //val wrapper = ProgramPreprocessor(osAppropriatePath)(args)
    val wrapper = ProgramPreprocessor(name, program)(JsonObject.empty)
    wrapper.json.as[ProgramPreprocessor].right.get shouldEqual wrapper
  }*/

  property("ProgramPreprocessor should split a single script into separate state and code objects") {


    val easyScript =
      s"""
         |var a = 0
         |
         |/**
         |* @param {Number}
         |* @param {Number}
         |**/
         |add = function(a,b) {
         |  return a + b
         |}
         |
         |/**
         |* @param {Number}
         |* @param {Number}
         |**/
         |sub = function(a,b) {
         |  return a - b
         |}
         |
         |/**
         |* @param {String}
         |* @param {String}
         |**/
         |concat = function(a,b) {
         |  return a + b
         |}
       """.stripMargin

    val preprocessor = ProgramPreprocessor(name, easyScript)(JsonObject.empty)
  }
}