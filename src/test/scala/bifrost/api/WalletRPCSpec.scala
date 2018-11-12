package bifrost.api

import java.io.File

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import bifrost.api.http.{WalletApiRouteRPC}
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.state.{BifrostState}
import bifrost.transaction.{BifrostTransaction}
import bifrost.wallet.BWallet
import bifrost.{BifrostGenerators, BifrostNodeViewHolder}
import io.circe.parser.parse
import org.scalatest.{Matchers, WordSpec}
import scorex.crypto.encode.Base58

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.io.Path
import scala.util.Try

/**
  * Created by cykoz on 7/3/2017.
  */
class WalletRPCSpec extends WordSpec
  with Matchers
  with ScalatestRouteTest
  with BifrostGenerators {

  val actorSystem = ActorSystem(settings.agentName)
  val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new BifrostNodeViewHolder(settings)))
  nodeViewHolderRef
  val route = WalletApiRouteRPC(settings, nodeViewHolderRef).route

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/wallet/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    )
  }

  implicit val timeout = Timeout(10.seconds)

  private def view() = Await.result((nodeViewHolderRef ? GetCurrentView)
    .mapTo[CurrentView[BifrostHistory, BifrostState, BWallet, BifrostMemPool]], 10.seconds)

  val publicKeys = Map(
    "investor" -> "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
    "producer" -> "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb",
    "hub" -> "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU"
  )

  var newPubKey: String = ""

  // Unlock Secrets
  val gw: BWallet = view().vault
  gw.unlockKeyFile(publicKeys("investor"), "genesis")
  gw.unlockKeyFile(publicKeys("producer"), "genesis")
  gw.unlockKeyFile(publicKeys("hub"), "genesis")

  "Wallet RPC" should {
    "Get balances" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "30",
           |   "method": "balances",
           |   "params": [{}]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }

//    "Generate a keyfile" in {
//      val requestBody = ByteString(
//        s"""
//           |{
//           |   "jsonrpc": "2.0",
//           |   "id": "30",
//           |   "method": "generateKeyfile",
//           |   "params": [{
//           |     "password": "testpassword"
//           |   }]
//           |}
//        """.stripMargin)
//      //println(requestBody)
//      httpPOST(requestBody) ~> route ~> check {
//        val res = parse(responseAs[String]).right.get
//        (res \\ "error").isEmpty shouldBe true
//        (res \\ "result").head.asObject.isDefined shouldBe true
//        newPubKey = ((res \\ "result").head \\ "publicKey").head.asString.get
//      }
//    }
//
//    "Unlock a keyfile" in {
//      val requestBody = ByteString(
//        s"""
//           |{
//           |   "jsonrpc": "2.0",
//           |   "id": "30",
//           |   "method": "unlockKeyfile",
//           |   "params": [{
//           |     "publicKey": "${newPubKey}",
//           |     "password": "testpassword"
//           |   }]
//           |}
//        """.stripMargin)
//      //println(requestBody)
//      httpPOST(requestBody) ~> route ~> check {
//        val res = parse(responseAs[String]).right.get
//        (res \\ "error").isEmpty shouldBe true
//        (res \\ "result").head.asObject.isDefined shouldBe true
//
//        //Manually deleting any newly created keyfiles from test keyfile directory (keyfiles/node1) except for the
//        //investor, producer and hub keyfiles
//        var d = new File("keyfiles/node1")
//        d.listFiles.foreach(x =>
//          if(x.toString != "keyfiles/node1/2018-07-06T15-51-30Z-6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ.json" &&
//          x.toString != "keyfiles/node1/2018-07-06T15-51-35Z-F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU.json" &&
//          x.toString != "keyfiles/node1/2018-07-06T15-51-33Z-A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb.json") {
//          val tempFile = new File(x.toString)
//          tempFile.delete()
//          })
//
//      }
//    }

    "Transfer some polys" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "30",
           |   "method": "transfer",
           |   "params": [{
           |     "recipient": "${publicKeys("investor")}",
           |     "amount": 5,
           |     "fee": 0,
           |     "data": ""
           |   }]
           |}
        """.stripMargin)
      //println(requestBody)
      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
        val txHash = ((res \\ "result").head \\ "id").head.asString.get
        val txInstance: BifrostTransaction = view().pool.getById(Base58.decode(txHash).get).get
        view().pool.remove(txInstance)
      }
    }

  }


  object WalletRPCSpec {
    val path: Path = Path("/tmp/scorex/test-data")
    Try(path.deleteRecursively())
  }
}
