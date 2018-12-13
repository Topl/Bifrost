package bifrost.contract

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.util.{ByteString, Timeout}
import bifrost.{BifrostApp, BifrostNodeViewHolder}
import bifrost.api.http.{AssetApiRoute, ContractApiRoute, WalletApiRouteRPC}
import bifrost.forging.ForgingSettings
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.network.BifrostNodeViewSynchronizer
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.state.BifrostState
import bifrost.wallet.BWallet
import com.oracle.truffle.api.CompilerDirectives
import com.oracle.truffle.api.frame.VirtualFrame
import com.oracle.truffle.api.instrumentation.StandardTags.CallTag
import com.oracle.truffle.api.instrumentation._
import io.circe
import io.circe.Json
import org.graalvm.polyglot.management.ExecutionListener
import org.graalvm.polyglot.{Context, Value}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import scala.language.existentials

case class ValkyrieFunctions() {

  val reserved: String =
    s"""
       |var assetCreated, assetTransferred, polyTransferred;
       |
     |this.createAssets = function(publicKey, asset, amount) {
       |  return assetCreated;
       |}
       |
     |this.transferAssets = function(publicKey, asset, amount, data) {
       |  return assetTransferred;
       |}
       |
     |this.transferPoly = function(publicKey, amount) {
       |  return polyTransferred;
       |}
   """.stripMargin
}

object ValkyrieFunctions {

  implicit lazy val settings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile("testnet-private.json")
  }

  implicit val actorSystem = ActorSystem(settings.agentName)
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = actorSystem.dispatcher
  val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new BifrostNodeViewHolder(settings)))
  nodeViewHolderRef

  val assetRoute = AssetApiRoute(settings, nodeViewHolderRef)(actorSystem).route

  val walletRoute = WalletApiRouteRPC(settings, nodeViewHolderRef)(actorSystem).route

  def assetHttpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/asset/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    )
  }

  def walletHttpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/walletrpc/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    )
  }

  implicit val timeout = Timeout(10.seconds)

  private def view() = Await.result((nodeViewHolderRef ? GetCurrentView)
    .mapTo[CurrentView[BifrostHistory, BifrostState, BWallet, BifrostMemPool]], 10.seconds)

  def createAsset(body: String): Unit = {
    val response: Future[HttpResponse] = Http().singleRequest(assetHttpPOST(ByteString(body)))
    response.onComplete {
      case Success(res) => println(res)
      case Failure(_) => println("Error completing request")
    }
  }

  def transferAsset(body: String): Unit = {
    val response: Future[HttpResponse] = Http().singleRequest(assetHttpPOST(ByteString(body)))
    response.onComplete {
      case Success(res) => println(res)
      case Failure(_) => println("Error completing request")
    }
  }

  def transferPoly(body: String): Unit = {
    val response: Future[HttpResponse] = Http().singleRequest(walletHttpPOST(ByteString(body)))
    response.onComplete {
      case Success(res) => println(res)
      case Failure(_) => println("Error completing request")
    }
  }

  println(s">>>>>>>>>> before apply")

  //noinspection ScalaStyle
  def apply(context: Context, params: String) = {

    val modifiedParams = {}

    println(s">>>>>>>>>>>>>>> createExecutionListener")
    /*val listener: ExecutionListener = ExecutionListener.newBuilder()
    .onEnter({
      e => val protocolFunction = e.getLocation.getCharacters.toString
        protocolFunction match {
          case "createAsset()" => println("success")
        }
    })
    .roots(true)
    .attach(context.getEngine)*/

    val truffleListener: ExecutionEventListener = {
      println(s">>>>>>> truffleListener")
      new ExecutionEventListener {
        println(s">>>>>>>>>> new ExecutionListener")
        override def onEnter(context: EventContext, frame: VirtualFrame): Unit = try {
          println(s">>>>>>>>> onReturnValue")
          val source: String = context.getInstrumentedSourceSection.getCharacters.toString
          source match {
            case "assetCreated" => CompilerDirectives.transferToInterpreter(); throw context.createUnwind("ac")
            case "assetTransferred" => CompilerDirectives.transferToInterpreter(); throw context.createUnwind("at")
            case "polyTransferred" => CompilerDirectives.transferToInterpreter(); throw context.createUnwind("pt")
          }
        } catch {
          case e: Exception =>
            e.getStackTrace
        }

        override def onReturnValue(context: EventContext, frame: VirtualFrame, result: Any): Unit = ???

        override def onReturnExceptional(context: EventContext, frame: VirtualFrame, exception: Throwable): Unit = ???

        import com.oracle.truffle.api.frame.VirtualFrame

        override def onUnwind(context: EventContext, frame: VirtualFrame, info: Object): Object = {
          info match {
            case "ac" => createAsset(params); ProbeNode.UNWIND_ACTION_REENTER
            case "at" => transferAsset(params); ProbeNode.UNWIND_ACTION_REENTER
            case "pt" => transferPoly(params); ProbeNode.UNWIND_ACTION_REENTER
          }
        }
      }
    }

    @TruffleInstrument.Registration(id = "Valkyrie", services = Array(classOf[ValkyrieListenerInstrument]))
    case class ValkyrieListenerInstrument() extends TruffleInstrument {
      println(s">>>>>>>>> ValkyrieListenerInstrument")
      override def onCreate(env: TruffleInstrument.Env): Unit = {
        println(s">>>>>>>> ValkyrieListenerInstrument")
        env.registerService(this)
        env.getInstrumenter.attachExecutionEventListener(SourceSectionFilter.newBuilder().tagIs(classOf[CallTag]).build(), truffleListener)
      }
    }
    ValkyrieListenerInstrument
  }
}