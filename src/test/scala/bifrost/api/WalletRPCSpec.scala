package bifrost.api

import java.io.File

import akka.actor.ActorRef
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import bifrost.BifrostGenerators
import bifrost.history.History
import bifrost.http.api.routes.WalletApiRoute
import bifrost.mempool.MemPool
import bifrost.modifier.ModifierId
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import bifrost.nodeView.GenericNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import bifrost.nodeView.{CurrentView, NodeViewHolderRef}
import bifrost.settings.BifrostContext
import bifrost.state.State
import bifrost.wallet.Wallet
import io.circe.parser.parse
import scorex.crypto.encode.Base58

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.io.Path
import scala.util.Try
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


class WalletRPCSpec extends AnyWordSpec
  with Matchers
  with ScalatestRouteTest
  with BifrostGenerators {

  val path: Path = Path("/tmp/bifrost/test-data")
  Try(path.deleteRecursively())

  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */
  // save environment into a variable for reference throughout the application
  protected val bifrostContext = new BifrostContext(settings, None)

  // Create Bifrost singleton actors
  private val nodeViewHolderRef: ActorRef = NodeViewHolderRef("nodeViewHolder", settings, bifrostContext)
  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */

  // setup route for testing
  val route: Route = WalletApiRoute(settings.restApi, nodeViewHolderRef).route

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/wallet/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  implicit val timeout: Timeout = Timeout(20.seconds)

  private def actOnCurrentView(v: CurrentView[History, State, Wallet, MemPool]): CurrentView[History, State, Wallet, MemPool] = v

  private def view() = Await.result(
    (nodeViewHolderRef ? GetDataFromCurrentView(actOnCurrentView)).mapTo[CurrentView[History, State, Wallet, MemPool]],
    20.seconds)

  val publicKeys = Map(
    "investor" -> "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
    "producer" -> "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb",
    "hub" -> "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU"
  )

  var newPubKey: String = ""

  // Unlock Secrets
  val gw: Wallet = view().vault
  gw.unlockKeyFile(publicKeys("investor"), "genesis")
  gw.unlockKeyFile(publicKeys("producer"), "genesis")
  gw.unlockKeyFile(publicKeys("hub"), "genesis")

  "WalletTrait RPC" should {
    "Get balances" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "balances",
           |   "params": [{
           |      "publicKeys": ["${publicKeys("investor")}"]
           |   }]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }

    "Transfer some arbits" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "transferArbits",
           |   "params": [{
           |      "sender": ["${publicKeys("investor")}", "${publicKeys("hub")}", "${publicKeys("producer")}"],
           |     "recipient": "${publicKeys("producer")}",
           |     "amount": 5,
           |     "fee": 0,
           |     "data": ""
           |   }]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
        //Removing transaction from mempool so as not to affect ProgramRPC tests
        val txHash = ((res \\ "result").head \\ "txHash").head.asString.get
        val txHashId = ModifierId(Base58.decode(txHash).get)
        val txInstance: Transaction = view().pool.getById(txHashId).get
        view().pool.remove(txInstance)
      }
    }

    "Create transfer arbits prototype" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "transferArbitsPrototype",
           |   "params": [{
           |   "sender": ["${publicKeys("investor")}", "${publicKeys("hub")}", "${publicKeys("producer")}"],
           |     "recipient": "${publicKeys("producer")}",
           |     "amount": 5,
           |     "fee": 0,
           |     "data": ""
           |   }]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }

    "Transfer some polys" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "transferPolys",
           |   "params": [{
           |      "sender": ["${publicKeys("investor")}", "${publicKeys("hub")}", "${publicKeys("producer")}"],
           |     "recipient": "${publicKeys("investor")}",
           |     "amount": 5,
           |     "fee": 0,
           |     "data": ""
           |   }]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
        val txHash = ((res \\ "result").head \\ "txHash").head.asString.get
        val txHashId = ModifierId(Base58.decode(txHash).get)
        val txInstance: Transaction = view().pool.getById(txHashId).get
        view().pool.remove(txInstance)
      }
    }

    "Create transfer polys prototype" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "transferPolysPrototype",
           |   "params": [{
           |   "sender": ["${publicKeys("investor")}", "${publicKeys("hub")}", "${publicKeys("producer")}"],
           |     "recipient": "${publicKeys("producer")}",
           |     "amount": 5,
           |     "fee": 0,
           |     "data": ""
           |   }]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }

    "Get open keyfiles" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "listOpenKeyfiles",
           |   "params": [{}]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asArray.isDefined shouldBe true
        (res \\ "result").head.as[List[String]].right.get == publicKeys.values.toList shouldBe true
      }
    }

    "Generate a keyfile" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "generateKeyfile",
           |   "params": [{
           |     "password": "testpassword"
           |   }]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
        newPubKey = ((res \\ "result").head \\ "publicKey").head.asString.get
      }
    }

    "Lock a keyfile" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "lockKeyfile",
           |   "params": [{
           |     "publicKey": "$newPubKey",
           |     "password": "testpassword"
           |   }]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }

    "Unlock a keyfile" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "unlockKeyfile",
           |   "params": [{
           |     "publicKey": "$newPubKey",
           |     "password": "testpassword"
           |   }]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true

        //Manually deleting any newly created keyfiles from test keyfile directory (keyfiles/node1) except for the
        //investor, producer and hub keyfiles
        val d = new File("keyfiles/node1")
        d.listFiles.foreach(x =>
          if(x.toString != "keyfiles/node1/2018-07-06T15-51-30Z-6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ.json" &&
          x.toString != "keyfiles/node1/2018-07-06T15-51-35Z-F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU.json" &&
          x.toString != "keyfiles/node1/2018-07-06T15-51-33Z-A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb.json") {
            val tempFile = new File(x.toString)
            tempFile.delete()
          }
        )
      }
    }
  }


  object WalletRPCSpec {
    val path: Path = Path("/tmp/bifrost/test-data")
    Try(path.deleteRecursively())
  }
}