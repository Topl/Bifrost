package bifrost.api

import java.io.File

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import bifrost.api.http.{WalletApiRoute}
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.state.{BifrostState}
import bifrost.transaction.{BifrostTransaction}
import bifrost.wallet.BWallet
import bifrost.{BifrostGenerators, BifrostNodeViewHolder}
import io.circe.parser.parse
import io.circe.syntax._
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
  val route = WalletApiRoute(settings, nodeViewHolderRef).route

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

    "Get balances by public key" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "30",
           |   "method": "balances",
           |   "params": [{"publicKey": "${publicKeys("hub")}"}]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
        val boxes = ((res \\ "result").head \\ "boxes").head.asArray
        boxes.get.foreach(b => (b \\ "proposition").head.asString.get shouldEqual publicKeys("investor"))
      }
    }

    "Transfer some arbits" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "30",
           |   "method": "transferArbits",
           |   "params": [{
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
        //Removing transaction from mempool so as not to affect ContractRPC tests
        val txHash = ((res \\ "result").head \\ "txHash").head.asString.get
        val txInstance: BifrostTransaction = view().pool.getById(Base58.decode(txHash).get).get
        view().pool.remove(txInstance)
      }
    }

    "Transfer some arbits from a specified public key in wallet" in {
      val requestBody = ByteString(
        s"""
           |{
           |
           |   "jsonrpc": "2.0",
           |   "id": "30",
           |   "method": "transferArbits",
           |   "params": [{
           |     "recipient": "${publicKeys("hub")}",
           |     "publicKeyToSendFrom": ["${publicKeys("investor")}"],
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
        val txInstance: BifrostTransaction = view().pool.getById(Base58.decode(txHash).get).get
        view().pool.remove(txInstance)
      }
    }

    "Transfer some arbits from a specified public key and specify a change address" in {
      val requestBody = ByteString(
        s"""
           |{
           |
           |   "jsonrpc": "2.0",
           |   "id": "30",
           |   "method": "transferArbits",
           |   "params": [{
           |     "recipient": "${publicKeys("hub")}",
           |     "publicKeyToSendFrom": ["${publicKeys("investor")}"],
           |     "publicKeyToSendChangeTo": "${publicKeys("producer")}",
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
        val txInstance: BifrostTransaction = view().pool.getById(Base58.decode(txHash).get).get
        view().pool.remove(txInstance)
      }
    }

    "Transfer some arbits and specify a change address but no sender" in {
      val requestBody = ByteString(
        s"""
           |{
           |
           |   "jsonrpc": "2.0",
           |   "id": "30",
           |   "method": "transferArbits",
           |   "params": [{
           |     "recipient": "${publicKeys("hub")}",
           |     "publicKeyToSendChangeTo": "${publicKeys("producer")}",
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
        val txInstance: BifrostTransaction = view().pool.getById(Base58.decode(txHash).get).get
        view().pool.remove(txInstance)
      }
    }

    "Transfer some polys" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "30",
           |   "method": "transferPolys",
           |   "params": [{
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
        val txInstance: BifrostTransaction = view().pool.getById(Base58.decode(txHash).get).get
        view().pool.remove(txInstance)
      }
    }
    "Transfer some polys from a specified public key in wallet" in {
      val requestBody = ByteString(
        s"""
           |{
           |
           |   "jsonrpc": "2.0",
           |   "id": "30",
           |   "method": "transferPolys",
           |   "params": [{
           |     "recipient": "${publicKeys("hub")}",
           |     "publicKeyToSendFrom": ["${publicKeys("investor")}"],
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
        val txInstance: BifrostTransaction = view().pool.getById(Base58.decode(txHash).get).get
        view().pool.remove(txInstance)
      }
    }

    "Transfer some polys from a specified public key and specify a change address" in {
      val requestBody = ByteString(
        s"""
           |{
           |
           |   "jsonrpc": "2.0",
           |   "id": "30",
           |   "method": "transferPolys",
           |   "params": [{
           |     "recipient": "${publicKeys("hub")}",
           |     "publicKeyToSendFrom": ["${publicKeys("investor")}"],
           |     "publicKeyToSendChangeTo": "${publicKeys("producer")}",
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
        val txInstance: BifrostTransaction = view().pool.getById(Base58.decode(txHash).get).get
        view().pool.remove(txInstance)
      }
    }

    "Transfer some polys and specify a change address but no sender" in {
      val requestBody = ByteString(
        s"""
           |{
           |
           |   "jsonrpc": "2.0",
           |   "id": "30",
           |   "method": "transferPolys",
           |   "params": [{
           |     "recipient": "${publicKeys("hub")}",
           |     "publicKeyToSendChangeTo": "${publicKeys("producer")}",
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
        val txInstance: BifrostTransaction = view().pool.getById(Base58.decode(txHash).get).get
        view().pool.remove(txInstance)
      }
    }

    "Get open keyfiles" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "30",
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
           |   "id": "30",
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
           |   "id": "30",
           |   "method": "lockKeyfile",
           |   "params": [{
           |     "publicKey": "${newPubKey}",
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
           |   "id": "30",
           |   "method": "unlockKeyfile",
           |   "params": [{
           |     "publicKey": "${newPubKey}",
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
        var d = new File("keyfiles/node1")
        d.listFiles.foreach(x =>
          if(x.toString != "keyfiles/node1/2018-07-06T15-51-30Z-6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ.json" &&
          x.toString != "keyfiles/node1/2018-07-06T15-51-35Z-F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU.json" &&
          x.toString != "keyfiles/node1/2018-07-06T15-51-33Z-A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb.json") {
            val tempFile = new File(x.toString)
            println(s"cleanup: deleting keyfiles")
            tempFile.delete()
          }
        )
      }
    }

    //    "Transfer some polys from a list of specified public keys in wallet" in {
    //      val requestBody = ByteString(
    //        s"""
    //           |{
    //           |
    //           |   "jsonrpc": "2.0",
    //           |   "id": "30",
    //           |   "method": "transferByKeys",
    //           |   "params": [{
    //           |     "recipient": "${publicKeys("hub")}",
    //           |     "publicKeysToSendFrom": [
    //           |         "${publicKeys("investor")}",
    //           |        "${publicKeys("hub")}"
    //           |     ],
    //           |     "amount": 5,
    //           |     "fee": 0,
    //           |     "data": ""
    //           |   }]
    //           |}
    //        """.stripMargin)
    //      //println(requestBody)
    //      httpPOST(requestBody) ~> route ~> check {
    //        val res = parse(responseAs[String]).right.get
    //        println(res)
    //        (res \\ "error").isEmpty shouldBe true
    //        (res \\ "result").head.asObject.isDefined shouldBe true
    //        val txHash = ((res \\ "result").head \\ "id").head.asString.get
    //        val txInstance: BifrostTransaction = view().pool.getById(Base58.decode(txHash).get).get
    //        view().pool.remove(txInstance)
    //      }
    //    }

  }


  object WalletRPCSpec {
    val path: Path = Path("/tmp/scorex/test-data")
    Try(path.deleteRecursively())
  }
}



//    "Transfer some polys" in {
//      val requestBody = ByteString(
//        s"""
//           |{
//           |   "jsonrpc": "2.0",
//           |   "id": "30",
//           |   "method": "transfer",
//           |   "params": [{
//           |     "recipient": "${publicKeys("hub")}",
//           |     "amount": 5,
//           |     "fee": 0,
//           |     "data": ""
//           |   }]
//           |}
//        """.stripMargin)
//      //println(requestBody)
//      httpPOST(requestBody) ~> route ~> check {
//        val res = parse(responseAs[String]).right.get
//        (res \\ "error").isEmpty shouldBe true
//        (res \\ "result").head.asObject.isDefined shouldBe true
//        val txHash = ((res \\ "result").head \\ "txHash").head.asString.get
//        val txInstance: BifrostTransaction = view().pool.getById(Base58.decode(txHash).get).get
//        view().pool.remove(txInstance)
//      }
//    }
//
//    "Transfer some polys from a specified public key in wallet" in {
//      val requestBody = ByteString(
//        s"""
//           |{
//           |
//           |   "jsonrpc": "2.0",
//           |   "id": "30",
//           |   "method": "transfer",
//           |   "params": [{
//           |     "recipient": "${publicKeys("hub")}",
//           |     "publicKeysToSendFrom": ["${publicKeys("investor")}"],
//           |     "publicKeyToSendChangeTo": "${publicKeys("investor")}",
//           |     "amount": 100000000,
//           |     "fee": 0,
//           |     "data": ""
//           |   }]
//           |}
//        """.stripMargin)
//      //println(requestBody)
//      httpPOST(requestBody) ~> route ~> check {
//        val res = parse(responseAs[String]).right.get
//        (res \\ "error").isEmpty shouldBe true
//        (res \\ "result").head.asObject.isDefined shouldBe true
//        val txHash = ((res \\ "result").head \\ "txHash").head.asString.get
//        val txInstance: BifrostTransaction = view().pool.getById(Base58.decode(txHash).get).get
//        view().pool.remove(txInstance)
//      }
//    }

//    "Transfer some polys from a list of specified public keys in wallet" in {
//      val requestBody = ByteString(
//        s"""
//           |{
//           |
//           |   "jsonrpc": "2.0",
//           |   "id": "30",
//           |   "method": "transferByKeys",
//           |   "params": [{
//           |     "recipient": "${publicKeys("hub")}",
//           |     "publicKeysToSendFrom": [
//           |         "${publicKeys("investor")}",
//           |        "${publicKeys("hub")}"
//           |     ],
//           |     "amount": 5,
//           |     "fee": 0,
//           |     "data": ""
//           |   }]
//           |}
//        """.stripMargin)
//      //println(requestBody)
//      httpPOST(requestBody) ~> route ~> check {
//        val res = parse(responseAs[String]).right.get
//        println(res)
//        (res \\ "error").isEmpty shouldBe true
//        (res \\ "result").head.asObject.isDefined shouldBe true
//        val txHash = ((res \\ "result").head \\ "txHash").head.asString.get
//        val txInstance: BifrostTransaction = view().pool.getById(Base58.decode(txHash).get).get
//        view().pool.remove(txInstance)
//      }
//    }