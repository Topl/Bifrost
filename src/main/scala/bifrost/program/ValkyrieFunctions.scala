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
import com.oracle.truffle.api.instrumentation._
import io.circe
import io.circe.JsonObject
import org.graalvm.polyglot.Context

import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import scala.language.existentials

case class ValkyrieFunctions() {

  val reserved: String =
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
       |this.transferAssets = function(amount, recipient, fee, issuer, assetCode, data) {
       |  this.amount = amount;
       |  this.recipient = recipient;
       |  this.fee = fee;
       |  this.issuer = issuer;
       |  this.assetCode = assetCode;
       |  this.data = data;
       |
       |  return assetTransferred;
       |}
       |
       |this.transferPolys = function(amount, recipient, fee, data) {
       |  this.amount = amount;
       |  this.recipient = recipient;
       |  this.fee = fee;
       |  this.data = data;
       |
       |  return polyTransferred;
       |}
   """.stripMargin

  object reservedFunctions extends Enumeration {
    val func: Value = Value
    val createAssets: Value = Value("createAssets")
    val transferAssets: Value = Value("transferAssets")
    val transferPolys: Value = Value("transferPolys")
    val transferArbits: Value = Value("transferArbits")
  }
}

object ValkyrieFunctions {

  implicit lazy val settings: ForgingSettings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile("testnet-private.json")
  }

  implicit val actorSystem: ActorSystem = ActorSystem(settings.agentName)
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
  val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new BifrostNodeViewHolder(settings)))
  nodeViewHolderRef

  val assetRoute: Route = AssetApiRoute(settings, nodeViewHolderRef)(actorSystem).route

  val walletRoute: Route = WalletApiRoute(settings, nodeViewHolderRef)(actorSystem).route

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
      uri = "/wallet/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    )
  }

  implicit val timeout: Timeout = Timeout(10.seconds)

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
  def apply(context: Context, params: JsonObject) = {

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
        override def onEnter(context: EventContext, frame: VirtualFrame): Unit = {
          println()
          println("Entered")
          println()
        }

        override def onReturnValue(context: EventContext, frame: VirtualFrame, result: Any): Unit = {
          println(s">>>>>>>>> onReturnValue")
          val source: String = context.getInstrumentedSourceSection.getCharacters.toString
          println(source)
          source match {
            case "assetCreated" => CompilerDirectives.transferToInterpreter(); throw context.createUnwind("ca")
            case "assetTransferred" => CompilerDirectives.transferToInterpreter(); throw context.createUnwind("ta")
            case "polyTransferred" => CompilerDirectives.transferToInterpreter(); throw context.createUnwind("tp")
          }
        }

        override def onReturnExceptional(context: EventContext, frame: VirtualFrame, exception: Throwable): Unit = ???

        import com.oracle.truffle.api.frame.VirtualFrame

        override def onUnwind(context: EventContext, frame: VirtualFrame, info: Object): Object = {
          info match {
            case "ca" => {
              println()
              println("Matched Valkyrie return type correctly")
              println()
              val jsonrpcParams =
                s"""
                   |{
                   |  "jsonprc": "2.0",
                   |  "method": "createAssets",
                   |  "params": $params
                   |}
                 """.stripMargin

              println(s">>>>>>> jsonrpcParams: $jsonrpcParams")
              createAsset(jsonrpcParams)
              ProbeNode.UNWIND_ACTION_REENTER
            }

            case "ta" => {

              val jsonrpcParams =
                s"""
                   |{
                   |  "jsonprc": "2.0",
                   |  "method": "transferAssets",
                   |  "params": $params
                   |}
                 """.stripMargin

              println(s">>>>>>> jsonrpcParams: $jsonrpcParams")
              transferAsset(jsonrpcParams)
              ProbeNode.UNWIND_ACTION_REENTER
            }

            case "tp" => {

              val jsonrpcParams =
                s"""
                   |{
                   |  "jsonprc": "2.0",
                   |  "method": "transferPolys",
                   |  "params": $params
                   |}
                 """.stripMargin

              println(s">>>>>>> jsonrpcParams: $jsonrpcParams")
              transferPoly(jsonrpcParams)
              ProbeNode.UNWIND_ACTION_REENTER
            }
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