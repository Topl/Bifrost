package bifrost.program

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.util.{ByteString, Timeout}
import bifrost.{BifrostLocalInterface, BifrostNodeViewHolder}
import bifrost.api.http.{AssetApiRoute, WalletApiRoute}
import bifrost.forging.ForgingSettings
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.state.BifrostState
import bifrost.wallet.BWallet
import com.oracle.truffle.api.CompilerDirectives
import com.oracle.truffle.api.frame.VirtualFrame
import com.oracle.truffle.api.instrumentation.StandardTags.CallTag
import bifrost.program.ValkyrieController
import com.oracle.js.parser.ir.Expression
import com.oracle.truffle.api.instrumentation._
import com.oracle.truffle.js.nodes.instrumentation.JSTags
import io.circe
import io.circe.JsonObject
import org.graalvm.polyglot.Context
import org.graalvm.polyglot.Source
import org.graalvm.polyglot._

import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import scala.language.existentials



//
//@TruffleInstrument.Registration(id = "Valkyrie", services = Array(classOf[ValkyrieController]))
//abstract class Valkyrie() extends TruffleInstrument {
//
//  var controller: Controller
//  override def onCreate(env: TruffleInstrument.Env): Unit = {
//    this.controller = new Controller(env.getInstrumenter)
//    env.registerService(controller)
//  }
//
//  case class Controller(var instrumenter: Instrumenter) extends ValkyrieController {
//
//    class ValkyrieListener extends ExecutionEventListener {
//
//      override def onEnter(context: EventContext, frame: VirtualFrame): Unit = {
//        val source: String = context.getInstrumentedSourceSection.getCharacters.toString
//        source match {
//          case "assetCreated" => CompilerDirectives.transferToInterpreter(); throw context.createUnwind("ca")
//          case "assetTransferred" => CompilerDirectives.transferToInterpreter(); throw context.createUnwind("ta")
//          case "polyTransferred" => CompilerDirectives.transferToInterpreter(); throw context.createUnwind("tp")
//        }
//
//      }
//
//      override def onReturnExceptional(context: EventContext, frame: VirtualFrame, exception: Throwable): Unit = ???
//
//      override def onReturnValue(context: EventContext, frame: VirtualFrame, result: Any): Unit = ???
//
//
//
//    }
//  }
//
//
//}


@TruffleInstrument.Registration(id = "Valkyrie", services = Array(classOf[Valkyrie]))
case class Valkyrie() extends TruffleInstrument {
  val id: String = Valkyrie.id

  override protected def onCreate(env: TruffleInstrument.Env): Unit = {
    println(">>>>>>>>>>>>>>>> Instrument onCreate")
    var builder = SourceSectionFilter.newBuilder()

    var filter = builder.tagIs(classOf[StandardTags.StatementTag]).build()

//    var filter = builder.tagIs(classOf[JSTags]).build()

    var instrumenter: Instrumenter = env.getInstrumenter()

    val truffleListener = new ExecutionEventListener {
      println(">>>>>>>>>>truffleListener creation")
      override def onEnter(context: EventContext, frame: VirtualFrame): Unit = {
        println()
        println("onEnter")
        println()
      }

      override def onReturnValue(context: EventContext, frame: VirtualFrame, result: Any): Unit = {
        println()
        println("onReturnValue")
        println()
      }

      override def onReturnExceptional(context: EventContext, frame: VirtualFrame, exception: Throwable): Unit = {
        println()
        println("onReturnExceptional")
        println()
      }
    }

    instrumenter.attachExecutionEventListener(filter, truffleListener)
    env.registerService(this)

  }

  def main(code: String): Unit = {
    println("Entered Valkyrie  main method")


    //    var context: Context = Context.create("js")

    var context: Context = Context.newBuilder("js")
      .allowAllAccess(true)
      .build()

    context.getEngine.getInstruments.get("Valkyrie").lookup(classOf[Valkyrie])

    context.eval(Source.create("js", code))
  }
}

object Valkyrie {
  val id: String = "Valkyrie"
  def mainObj(code: String): Unit = {
    println("Entered Valkyrie object main method")


//    var context: Context = Context.create("js")

    var context: Context = Context.newBuilder("js")
      .allowAllAccess(true)
      .build()
    context.getEngine.getInstruments.get("Valkyrie").lookup(classOf[Valkyrie])

    context.eval(Source.create("js", code))
  }
}