package co.topl.program

import co.topl.nodeView.state.box.{CodeBox, StateBox}
import co.topl.{BifrostGenerators, ValidGenerators}
import io.circe.JsonObject
import io.circe.syntax._
import org.graalvm.polyglot.Context
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class ProgramMethodSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Can call a function from a program") {
    forAll(programGen) {
      c: Program => {

        val party = propositionGen.sample.get
        val params = JsonObject.empty

        val prop = c.parties.head._1

        val state = c.executionBuilderObj.core.variables

        val stateTwo = s"""{ "b": 0 }""".asJson
        val stateThree = s"""{ "c": 0 }""".asJson

        val stateBox = StateBox(prop, 0L, programIdGen.sample.get, state)
        val stateBoxTwo = StateBox(prop, 1L, programIdGen.sample.get, stateTwo)
        val stateBoxThree = StateBox(prop, 2L, programIdGen.sample.get, stateThree)
        val codeBox = CodeBox(prop, 3L, programIdGen.sample.get, Seq("function inc() { a += 1; return a }"), Map("inc" -> Seq()))

        val stateBoxes = Seq(stateBox, stateBoxTwo, stateBoxThree)

        val result = Program.execute(stateBoxes, Seq(codeBox), "inc")(party)(params).get

        result.hcursor.get[Int]("a").right.get shouldEqual 1
      }
    }
  }

  property("Calling getFromState correctly reads from a read only StateBox") {
    forAll(programGen) {
      c: Program => {

        val prop = c.parties.head._1

        val state = c.executionBuilderObj.core.variables

        val stateTwo = s"""{ "b": 4 }""".asJson
        val stateThree = s"""{ "c": 7 }""".asJson

        val stateBox = StateBox(prop, 0L, programIdGen.sample.get, state)
        val stateBoxTwo = StateBox(prop, 1L, programIdGen.sample.get,stateTwo)
        val stateBoxThree = StateBox(prop, 2L, programIdGen.sample.get,stateThree)
        val codeBox = CodeBox(prop, 3L, programIdGen.sample.get,Seq(
          "function changeState(uuid, value, state) { state = getFromState(uuid, value) }"
        ), Map("changeState" -> Seq("String", "String", "String")))

        val stateBoxes = Seq(
          (stateBox, stateBox.value),
          (stateBoxTwo, stateBoxTwo.value),
          (stateBoxThree, stateBoxThree.value)
        )

        val args = JsonObject.fromMap(Map(
            "uuid" -> s"_${stateBoxes.drop(1).head._2.toString.replace("-","_")}".asJson,
            "value" -> "b".asJson,
            "state" -> "a".asJson
          ))


        val mutableState = stateBoxes.head._1.state.asObject.get.toMap
        val programCode: String = Seq(codeBox).foldLeft("")((a,b) => a ++ b.code.foldLeft("")((a,b) => a ++ (b + "\n")))

        val jsre: Context = Context.create("js")
        val bindings = jsre.getBindings("js")

        //Pass in JSON objects for each read-only StateBox
        stateBoxes.tail.map{ sb =>
          val formatteId: String = "_" + sb._2.toString.replace("-", "_")
          jsre.eval("js", s"""var $formatteId = JSON.parse(${sb._1.state})""")
        }

        //Inject function to read from read only StateBoxes
        val getFromState =
          s"""
             |function getFromState(uuid, value) {
             |  return this[uuid][value]
             |}
       """.stripMargin

        jsre.eval("js", getFromState)

        //Pass in writable state and functions
        mutableState.foreach(s => bindings.putMember(s._1, s._2))
        jsre.eval("js", programCode)
        jsre.eval("js", s"""a = getFromState(${args("uuid").get}, ${args("value").get})""")

        //Return entire state of the evaluation
        val output: Map[String, String] = bindings.getMemberKeys.toArray.map(k => k.toString -> bindings.getMember(k.toString).toString).toMap
        //val output: Map[String, String] = mutableState.map(s => s._1 -> bindings.getMember(s._1).toString)

        output("a").toInt shouldEqual 4

        //val result = Program.execute(stateBoxes, Seq(codeBox), "getFromState")(party)(params)
      }
    }
  }

  property("Changing the type of a variable in a mutable StateBox will error") {
    forAll(programGen) {
      c: Program => {
        val party = propositionGen.sample.get
        val params = JsonObject.empty

        val prop = c.parties.head._1

        val state = c.executionBuilderObj.core.variables

        val stateTwo = s"""{ "b": 0 }""".asJson
        val stateThree = s"""{ "c": 0 }""".asJson

        val stateBox = StateBox(prop, 0L, programIdGen.sample.get, state)
        val stateBoxTwo = StateBox(prop, 1L, programIdGen.sample.get, stateTwo)
        val stateBoxThree = StateBox(prop, 2L, programIdGen.sample.get, stateThree)
        val codeBox = CodeBox(prop, 3L, programIdGen.sample.get, Seq(
          s"""function changeType() {
             |  return a = "wrong"
             |}
           """.stripMargin), Map("changeType" -> Seq()))

        val stateBoxes = Seq(stateBox, stateBoxTwo, stateBoxThree)

        intercept[ClassCastException] {
          Program.execute(stateBoxes, Seq(codeBox), "changeType")(party)(params).get
        }
      }
    }
  }

  property("Removing a mutable state variable during execution will result in an error") {
    forAll(programGen) {
      c: Program => {
        val party = propositionGen.sample.get
        /*val params = JsonObject.fromMap(
          Map("newStatus" -> stringGen.sample.get.asJson))
         */
        val params = JsonObject.empty

        val prop = party

        val state = c.executionBuilderObj.core.variables

        val stateTwo = s"""{ "b": 0 }""".asJson
        val stateThree = s"""{ "c": 0 }""".asJson

        val stateBox = StateBox(prop, 0L, programIdGen.sample.get, state)
        val stateBoxTwo = StateBox(prop, 1L, programIdGen.sample.get, stateTwo)
        val stateBoxThree = StateBox(prop, 2L, programIdGen.sample.get, stateThree)
        val codeBox = CodeBox(prop, 3L, programIdGen.sample.get, Seq(
          s"""function deleteVar() {
             |  delete global.a
             |}
           """.stripMargin), Map("deleteVar" -> Seq()))

        val stateBoxes = Seq(stateBox, stateBoxTwo, stateBoxThree)

        intercept[NoSuchElementException] {
          Program.execute(stateBoxes, Seq(codeBox), "deleteVar")(party)(params).get
        }
      }
    }
  }

  property("Calling a function with mismatched argument types should fail") {
    forAll(programGen) {
      c: Program => {

        val party = propositionGen.sample.get

        val params = JsonObject.fromMap(Map("a" -> "2".asJson, "b" -> "2".asJson))

        val prop = c.parties.head._1

        val state = c.executionBuilderObj.core.variables

        val stateTwo = s"""{ "b": 0 }""".asJson
        val stateThree = s"""{ "c": 0 }""".asJson

        val stateBox = StateBox(prop, 0L, programIdGen.sample.get, state)
        val stateBoxTwo = StateBox(prop, 1L, programIdGen.sample.get, stateTwo)
        val stateBoxThree = StateBox(prop, 2L, programIdGen.sample.get, stateThree)
        val codeBox = CodeBox(prop, 3L, programIdGen.sample.get, Seq(
          s"""add = function(a,b) {
             |  return a + b
             |}
           """.stripMargin), Map("add" -> Seq("Number", "Number")))

        val stateBoxes = Seq(stateBox, stateBoxTwo, stateBoxThree)

        intercept[Exception] {
          Program.execute(stateBoxes, Seq(codeBox), "add")(party)(params).get
        }
      }
    }
  }

  /*property("Can call createAssets protocol level function from a program") {
    forAll(programGen) {
      c: Program => {
        val party = propositionGen.sample.get
        val params = JsonObject.fromMap(
          Map(
            "issuer" -> stringGen.sample.get.asJson,
            "to" -> stringGen.sample.get.asJson,
            "assetCode" -> stringGen.sample.get.asJson,
            "amount" -> positiveTinyIntGen.sample.get.asJson))

        println(s"createAssets params: ${params}")

        val result = Program.execute(c, "createAssets")(party)(params)
        println(s"test result: $result")
        assert(result.isSuccess)
      }
    }
  }*/

  /*property("Can call transferAssets protocol level function from a program") {
    forAll(programGen) {
      c: Program => {
        val party = propositionGen.sample.get
        val params = JsonObject.fromMap(
          Map(
            "amount" -> positiveTinyIntGen.sample.get.asJson,
            "recipient" -> stringGen.sample.get.asJson,
            "issuer" -> stringGen.sample.get.asJson,
            "asset" -> stringGen.sample.get.asJson,
            "data" -> stringGen.sample.get.asJson))

        val result = Program.execute(c, "transferAssets")(party)(params)
        println(result)
        assert(result.isSuccess)
      }
    }
  }*/

  /*property("Can call polyTransfer protocol level function from a program") {
    forAll(programGen) {
      c: Program => {
        val party = propositionGen.sample.get
        val params = JsonObject.fromMap(
          Map(
            "publicKey" -> stringGen.sample.get.asJson,
            "amount" -> positiveTinyIntGen.sample.get.asJson))

        val result = Program.execute(c, "transferPolys")(party)(params)
        println(result)
        assert(result.isSuccess)
      }
    }
  }*/

    /*val name: String = "assetCreation"
    val assetCode: String = "Wheat"
    val effectiveTimestamp = Instant.now.toEpochMilli
    val expirationTimestamp = Instant.now.toEpochMilli + 10000L
    val program: String =
      s
         |this.$name = function(){
         |    this.programEffectiveTime = $effectiveTimestamp;
         |    this.programExpirationTime = $expirationTimestamp;
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

    val result = Program.execute(program, "createAsset")(party)(params)
    println(s">>>>>>>> Result: $result")*/
}