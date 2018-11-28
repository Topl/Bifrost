package bifrost.contract

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import bifrost.{BifrostApp, BifrostNodeViewHolder}
import bifrost.api.http.{AssetApiRoute, ContractApiRoute, WalletApiRoute}
import bifrost.forging.ForgingSettings
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.network.BifrostNodeViewSynchronizer
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.state.BifrostState
import bifrost.wallet.BWallet
import io.circe
import io.circe.Json
import org.graalvm.polyglot.Value

import scala.concurrent.Await
import scala.concurrent.duration._

case class ValkyrieFunctions() {

  implicit lazy val settings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(BifrostApp.settingsFilename)
  }

  val actorSystem = ActorSystem(settings.agentName)
  val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new BifrostNodeViewHolder(settings)))
  nodeViewHolderRef

  val assetRoute = AssetApiRoute(settings, nodeViewHolderRef)(actorSystem).route

  val walletRoute = WalletApiRoute(settings, nodeViewHolderRef)(actorSystem).route

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
      uri = "/asset/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    )
  }

  implicit val timeout = Timeout(10.seconds)

  private def view() = Await.result((nodeViewHolderRef ? GetCurrentView)
    .mapTo[CurrentView[BifrostHistory, BifrostState, BWallet, BifrostMemPool]], 10.seconds)

  class ProtocolFunctions() {
    val test: String = "test"


    def createAsset(body: String): Json = {
      assetHttpPOST(ByteString(body))
    }
  }

  def createBindings(bindings: Value): Unit = {
    bindings.putMember("test", new ProtocolFunctions)
  }
}

object ValkyrieFunctions {

  val reserved: String =
    s"""
       |this.createAsset = function(publicKey, asset, amount) {
       |  return this;
       |}
       |
       |this.transferAsset = function(publicKey, asset, amount) {
       |  return this;
       |}
       |
       |this.transferPoly = function(publicKey, amount) {
       |  return this;
       |}
     """.stripMargin
}