package bifrost.program


import java.lang

import bifrost.{BifrostGenerators, ValidGenerators}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import org.graalvm.polyglot.Context
import com.oracle.js.parser.{ErrorManager, Parser, ScriptEnvironment, Source}
import com.oracle.js.parser.ir.{BlockExpression, Expression, ExpressionStatement, FunctionNode, LexicalContext, Node, VarNode}
import com.oracle.js.parser.ir.visitor.NodeVisitor

import scala.util.Try
import java.util.Collection

import com.oracle.truffle.api.utilities.JSONHelper
import com.oracle.truffle.api.utilities.JSONHelper.{JSONObjectBuilder, JSONStringBuilder}
import com.oracle.truffle.js.parser.GraalJSParserHelper
import com.oracle.truffle.js.runtime.GraalJSParserOptions
import io.circe.Json
import io.circe.syntax._

import scala.collection.mutable
import scala.util.parsing.json.JSONObject

class TruffleSpec extends PropSpec
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  val name = "test"

  val testScript =
    s"""
       |var a = 0
       |var b = 1
       |
       |add = function() {
       |  a += 1
       |  b += 1
       |}
     """.stripMargin

  val script =
    s"""
       |var outside = "a"
       |
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

  val scriptEnv: ScriptEnvironment = ScriptEnvironment.builder
    .ecmaScriptVersion(8)
    .constAsVar(false)
    .earlyLvalueError(true)
    .emptyStatements(false)
    .syntaxExtensions(true)
    .scripting(false)
    .shebang(false)
    .strict(true)
    .functionStatementBehavior(ScriptEnvironment.FunctionStatementBehavior.ERROR)
    .build()

  val errManager = new ErrorManager.ThrowErrorManager
  val src = Source.sourceFor("script", testScript)
  val parser: Parser = new Parser(scriptEnv, src, errManager)
  val parsed = parser.parse()

  println(s"parsed: ${parsed.toString()}")

  def varList(node: FunctionNode): Json = {

    val jsre: Context = Context.create("js", testScript)

    var vars = scala.collection.mutable.Map[String, Json]()

    node.getBody.accept(new NodeVisitor[LexicalContext](new LexicalContext) {

      /*override def enterVarNode(varNode: VarNode): Boolean = {
        if(varNode.isInstanceOf[VarNode])
          {
            println(s"varNode.getName: ${varNode.getName}")
            println(s"varNode.getInit: ${varNode.getInit}")
            true
          }
        false
      }*/
      override def leaveVarNode(varNode: VarNode): VarNode = {
        println(s"getAssignmentSource: ${varNode.getAssignmentSource}")
        println(s"getStart: ${varNode.getStart}")
        println(s"getName: ${varNode.getName}")
        println(s"getInit: ${varNode.getInit}")
        println(s"varNode: ${varNode.toString()}")
        println(s"tokenType: ${varNode.tokenType.getName}")

        val name: String = varNode.getName.getName
        val init = varNode.getInit.toString(true)
        println(s"init with TypeInfo: $init")
        val value = jsre.eval("js", s"typeof $name").asString match {
          case "number" => vars += (name -> varNode.getInit.asInstanceOf[Double].asJson)
          case _ => vars += (name -> varNode.getInit.toString.asJson)
        }
        varNode
      }

    })
    vars.toMap.asJson
  }



  def functionList(node: FunctionNode): Node = {

    node.getBody.accept(new NodeVisitor[LexicalContext](new LexicalContext) {

      override def leaveFunctionNode(functionNode: FunctionNode): Node = {
        println(s"getKind: ${functionNode.getKind}")
        println(s"getBody: ${functionNode.getBody}")
        println(s"finish: ${functionNode.getFinish}")
        println(s"getLineNumber: ${functionNode.getLineNumber}")
        println(s"functionNode: ${functionNode.toString()}")
        println(s"tokenType: ${functionNode.tokenType().toString}")
        functionNode
      }
    })
  }



  //noinspection ScalaStyle
  {
    println("-------------------------------------------")
    println(s"varList: ${varList(parsed).toString}")
    println("-------------------------------------------")
    println(s"nodeList: ${functionList(parsed).toString}")
    println("-------------------------------------------")
    println(parsed.getBody.getStatements)
  }

  /*property("Script should be parsed into a list of variables") {

    val variables = Seq(varList(parsed).toString)
    variables shouldEqual Seq("var a = 0")

  }

  property("Script should be parsed into a list of functions") {

    val functions = Seq(functionList(parsed))
    functions shouldEqual Seq("add = function() { a = 2 + 2 }")
  }*/



  //val graalJSParserOptions = GraalJSParserOptions
  //GraalJSParserHelper.parseToJSON(testScript, "testScript", true, scriptEnv)



  val jsre: Context = Context.create("js")

  //val output = jsre.eval("js", testScript)
  /*Try {
    val add = jsre.eval("js", "add()")
  }*/
  //val outputBindings = jsre.getBindings("js")
  //val outputSource = Source.sourceFor("output", output.asString())
  //val outputParser = new Parser(scriptEnv, outputSource, errManager)
  //val parsedOutput = parser.parse()

  //println(s"outputBindings: ${outputBindings.getMember("add")}")
  //println(s"${outputBindings.getMember("a")}")
  //println(s"parsedOutput: ${parsedOutput.toString()}")

  println("-------------------------------------------")
  println(s"varList last: ${varList(parsed).toString}")

  //println(s"cast as Map: ${varList(parsed).as[Map[String, String]]}")

}