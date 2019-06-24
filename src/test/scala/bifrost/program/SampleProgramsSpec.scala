package bifrost.program

import bifrost.{BifrostGenerators, ValidGenerators}
import com.oracle.truffle.api.TruffleLanguage
import com.oracle.truffle.api.instrumentation.TruffleInstrument
import org.graalvm.polyglot.Context
import org.scalatest.{Matchers, PropSpec}
import org.graalvm.polyglot._
import org.graalvm.polyglot.management._
import bifrost.program.Valkyrie

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
       |var c = 1
       |var d = 2
       |var e = createAssets()
     """.stripMargin

  val testValkyrie =
    s"""
       |this.assetCreated = {};
       |this.assetTransferred = {};
       |this.polyTransferred = {};
       |
       |this.createAssets = function(issuer, to, amount, assetCode, fee, data) {
       |  this.issuer = issuer;
       |  this.to = to;
       |  this.amount = amount;
       |  this.assetCode = assetCode;
       |  this.fee = fee;
       |  this.data = data;
       |
       |  return assetCreated;
       |}
       |
       |function create() {
       |  a = 2 + 2
       |  var res = createAssets("a", "b", 10, "testAssets", 0, "")
       |  }
     """.stripMargin


//  println("")
//  val jsre: Context = Context.create("js")
//
//  val output = jsre.eval("js", testScriptCaller)
//
//  jsre.eval("js", "add()")
//
//  val outputBindings = jsre.getBindings("js")
//
//
///////////////////////
//
//  val bindingsKeys: Set[String] = asScalaSet(outputBindings.getMemberKeys).map(identity)(collection.breakOut) //Set[String]
//
//  println(bindingsKeys)
//
//  bindingsKeys.foreach( x => {
//    println(x + "\t" + outputBindings.getMember(x))
////    jsre2.getBindings("js").putMember(x, outputBindings.getMember(x))
//  }
//  )
//
//  jsre.close()





  /**Valkyrie Test*/
  println()
  println("Valkyrie Tests")


  var jsre_valk = Context.newBuilder()
    .allowAllAccess(true)
    //.option("engine.InstrumentExceptionsAreThrown", "true")
    .build()


  println(jsre_valk.getEngine().getInstruments().get("Valkyrie"))
  jsre_valk.getEngine().getInstruments().get("Valkyrie").lookup(classOf[Object])
  jsre_valk.initialize("js")
  jsre_valk.enter()


//  ExecutionListener.newBuilder().statements(true).expressions(true).roots(true).attach(jsre_valk.getEngine)



  //Evaluation
  jsre_valk.eval("js", testValkyrie)
  val output_valk = jsre_valk.eval("js", "create()")

  println(jsre_valk.getBindings("js").getMemberKeys)
  println(jsre_valk.getBindings("js").getMember("a"))




  /** Valkyrie with Graal Execution Listener*/
//  val context: Context = Context.create("js")
//  val listener: ExecutionListener = ExecutionListener.newBuilder()
//    .onEnter((e) => {
//      println("HELLO GUY")
//      println(e.getLocation.getCharacters)
////      println(e.getReturnValue)
//
//    })
////    .statements(true)
////    .roots(true)
//    .expressions(true)
//    .attach(context.getEngine())
//  context.eval("js", testValkyrie)
//
//  println()
//  println(">>>>>>>>>>>Evaluate here<<<<<<<<<<<")
//  context.eval("js", "create()")
//  listener.close()
}
