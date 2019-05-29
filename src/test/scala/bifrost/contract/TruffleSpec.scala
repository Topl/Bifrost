package bifrost.contract


import bifrost.{BifrostGenerators, ValidGenerators}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import com.oracle.js.parser.{ErrorManager, Parser, ScriptEnvironment, Source}
import com.oracle.js.parser.ir.{BlockExpression, Expression, FunctionNode, LexicalContext, Node}
import com.oracle.js.parser.ir.visitor.NodeVisitor

class TruffleSpec extends PropSpec
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  val name = "test"

  val testScript =
    s"""
       |var test = function() {
       |  return 1;
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
         |  this.contract = function() {
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
  val src = Source.sourceFor("script", script)
  val parser: Parser = new Parser(scriptEnv, src, errManager)
  val parsed = parser.parse()

  println(s"parsed: ${parsed.getBody.toString()}")

  def functionList(node: FunctionNode): Node = {

    node.getBody.accept(new NodeVisitor[LexicalContext](new LexicalContext) {

      override def leaveFunctionNode(functionNode: FunctionNode): Node = {
        println(s"getKind: ${functionNode.getKind}")
        println(s"getBody: ${functionNode.getBody}")
        println(s"getLineNumber: ${functionNode.getLineNumber}")
        println(s"functionNode: ${functionNode.toString()}")
        functionNode
      }
    })
  }

  println(s"nodeList: ${functionList(parsed).toString}")

  //noinspection ScalaStyle
  {
    println("-------------------------------------------")
    println(parsed.getBody.getStatements)
  }

}
