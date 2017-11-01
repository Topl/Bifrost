package bifrost.api

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import bifrost.{BifrostGenerators, BifrostNodeViewHolder}
import bifrost.api.http.AssetApiRoute
import bifrost.forging.ForgingSettings
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.network.PeerMessageManager
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.state.BifrostState
import bifrost.wallet.BWallet
import io.circe
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.io.Path
import scala.util.{Random, Try}

/**
  * Created by cykoz on 7/3/2017.
  */
class AssetRPCSpec extends WordSpec
  with Matchers
  with ScalatestRouteTest
  with BeforeAndAfterAll
  with BifrostGenerators {

  val actorSystem = ActorSystem(settings.agentName)
  val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new BifrostNodeViewHolder(settings)))
  nodeViewHolderRef
  val route = AssetApiRoute(settings, nodeViewHolderRef).route

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/asset/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    )
  }

  implicit val timeout = Timeout(10.seconds)

  private def view() = Await.result((nodeViewHolderRef ? GetCurrentView)
    .mapTo[CurrentView[BifrostHistory, BifrostState, BWallet, BifrostMemPool]], 10 seconds)

  val publicKeys = Map(
    "investor" -> "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
    "producer" -> "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb",
    "hub" -> "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU"
  )

  // Unlock Secrets
  val gw = view().vault
  // gw.unlockKeyFile(publicKeys("investor"), "genesis")
  gw.unlockKeyFile(publicKeys("producer"), "genesis")
  gw.unlockKeyFile(publicKeys("hub"), "genesis")

  "Asset RPC" should {
    "Redeem some assets" in {
      val requestBody = ByteString(
        s"""
          |{
          |   "jsonrpc": "2.0",
          |   "id": "30",
          |   "method": "redeemAssets",
          |   "params": [{
          |     "signingPublicKey": "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ"
          |   }]
          |}
        """.stripMargin)
    }
  }

  override def afterAll(): Unit = {
    val path: Path = Path ("/tmp/scorex/test-data")
    Try(path.deleteRecursively())
  }
}

object AssetRPCSpec {
  val path: Path = Path ("/tmp/scorex/test-data")
  Try(path.deleteRecursively())
}