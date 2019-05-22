package bifrost.contract


import bifrost.{BifrostGenerators, ValidGenerators}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import org.graalvm.polyglot._
import com.oracle.js.parser.{ErrorManager, Parser, ScriptEnvironment, Source}

class TruffleSpec extends PropSpec
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  val script =
    """
      |var x = 1;
      |var y = "test";
      |function z() { return x; };
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

  println(parsed.getBody.getStatements)

}
