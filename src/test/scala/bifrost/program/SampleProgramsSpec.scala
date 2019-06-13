package bifrost.program

import bifrost.{BifrostGenerators, ValidGenerators}
import org.graalvm.polyglot.Context
import org.scalatest.{Matchers, PropSpec}

import scala.collection.JavaConverters._

class SampleProgramsSpec extends PropSpec
  with Matchers
  with BifrostGenerators
  with ValidGenerators {


  //Script to run
  val testScriptCaller =
    s"""
       |var a = 0
       |var b = 1
       |
       |var add = function() {
       |  a = 2 + 2
       |  b = a + 2
       |  c = sub()
       |  return 11
       |}
       |
       |function sub() {
       |  return 5
       |}
       |
       |var addResult = add()
       |
     """.stripMargin

  val testScriptCallee =
    s"""
       |var c = a
       |var d = b
     """.stripMargin

  val testValkyrie =
    s"""
       |function create() {
       |  createAssets("a", "b", 10, "testAssets", 0, "")
       |  }
     """.stripMargin
/*
  //Preprocessing
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
  val src = Source.sourceFor("script", testScriptCaller)
  val parser: Parser = new Parser(scriptEnv, src, errManager)
  val parsed = parser.parse()

  println(s"parsed: ${parsed.toString()}")

  def varList(node: FunctionNode): Node = {

    var vars: Seq[String] = Seq("")

    node.getBody.accept(new NodeVisitor[LexicalContext](new LexicalContext) {

      override def enterVarNode(varNode: VarNode): Boolean = {
        if(varNode.isInstanceOf[VarNode])
        {
          println(s"varNode.getInit: ${varNode.getInit}")
          true
        }
        false
      }
      override def leaveVarNode(varNode: VarNode): VarNode = {
        println(s"getAssignmentSource: ${varNode.getAssignmentSource}")
        println(s"getStart: ${varNode.getStart}")
        println(s"getInit: ${varNode.getInit}")
        println(s"varNode: ${varNode.toString()}")
        varNode
      }

    })
  }

  def functionList(node: FunctionNode): Node = {

    node.getBody.accept(new NodeVisitor[LexicalContext](new LexicalContext) {

      override def leaveFunctionNode(functionNode: FunctionNode): Node = {
        println(s"functionNode: ${functionNode.toString()}")
        functionNode
      }
    })
  }
*/

  println("")
  val jsre: Context = Context.create("js")

  val output = jsre.eval("js", testScriptCaller)

  jsre.eval("js", "add()")

  val outputBindings = jsre.getBindings("js")

//  jsre.close()

/////////////////////

  val jsre2: Context = Context.create("js")

  val bindingsKeys: Set[String] = asScalaSet(outputBindings.getMemberKeys).map(identity)(collection.breakOut) //Set[String]

  println(bindingsKeys)

  bindingsKeys.foreach( x => {
    println(x + "\t" + outputBindings.getMember(x))
//    jsre2.getBindings("js").putMember(x, outputBindings.getMember(x))
  }
  )

  jsre.close()




//  jsre2.getBindings("js").putMember("jsobj", output.asString)

//  val output2 = jsre2.eval("js", testScriptCallee)
//
//  val outputBindings2 = jsre2.getBindings("js")

//  println(s"outputBindings: c = ${outputBindings2.getMember("c")}")


  //Valkyrie test

  val params =
    s"""
       |[
       |  "to" : "a"
       |]
     """.stripMargin
  val jsreValk: Context = Context.create("js")

  jsreValk.eval("js", testValkyrie)

  ValkyrieFunctions(jsreValk, null)



}
