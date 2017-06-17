package bifrost.api

import java.time.Instant
import java.util.concurrent.ConcurrentLinkedDeque

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import akka.http.scaladsl.model.{HttpEntity, HttpRequest, StatusCodes}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.server._
import akka.pattern.ask
import Directives._
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.{ByteString, Timeout}
import bifrost.BifrostNodeViewHolder
import bifrost.api.http.ContractApiRoute
import bifrost.blocks.BifrostBlock
import bifrost.forging.ForgingSettings
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.scorexMod.GenericNodeViewSynchronizer.{GetLocalObjects, ResponseFromLocal}
import bifrost.state.{BifrostState, BifrostStateChanges}
import bifrost.transaction.{ProfileTransaction, Role}
import bifrost.transaction.box.{ArbitBox, ContractBox, PolyBox, ProfileBox}
import bifrost.wallet.BWallet
import com.google.common.primitives.Ints
import io.circe
import scorex.core.settings.Settings
import io.circe._
import io.circe.parser._
import io.circe.generic.auto._
import io.circe.syntax._
import scorex.core.NodeViewModifier
import scorex.core.api.http.SuccessApiResponse
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.encode.Base58
import java.time.Clock

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.io.Path
import scala.util.{Failure, Random, Success, Try}

/**
  * Created by cykoz on 6/13/2017.
  */

class ContractRPCSpec extends WordSpec
  with Matchers
  with ScalatestRouteTest
  with BeforeAndAfterAll {

  import ContractRPCSpec._

  val actorSystem = ActorSystem(settings.agentName)
  val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(classOf[BifrostNodeViewHolder], settings))
  nodeViewHolderRef
  val route = ContractApiRoute(settings, nodeViewHolderRef).route
  println(settings.toString)

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/contract/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    )
  }

  implicit val timeout = Timeout(5.seconds)
  private def view() = Await.result((nodeViewHolderRef ? GetCurrentView)
    .mapTo[CurrentView[BifrostHistory, BifrostState, BWallet, BifrostMemPool]], 5 seconds)

  "Contract RPC" should {
    "return role or error" in {
      val requestBody = ByteString("""
        |{
        |  "jsonrpc": "2.0",
        |  "id": "16",
        |  "method": "getRole",
        |  "params": [{
        |      "publicKey": "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU"
        |  }]
        |}
        |""".stripMargin)
      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").head.asObject.isDefined shouldEqual true
        (res \\ "result").isEmpty shouldEqual true
      }
    }

    "Create a role" in {
      val requestBody = ByteString("""
        |{
        |  "jsonrpc": "2.0",
        |  "id": "16",
        |  "method": "declareRole",
        |  "params": [{
        |        "publicKey": "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
        |        "role": "investor"
        |    }, {
        |        "publicKey": "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU",
        |        "role": "hub"
        |    }, {
        |        "publicKey": "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb",
        |        "role": "producer"
        |    }]
        |}
        |""".stripMargin)
      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asArray.isDefined shouldEqual true
        (res \\ "error").isEmpty shouldEqual true

        val state = view().state
        val wallet = view().vault
        val profileBoxes = Seq(
          ProfileBox(wallet.secrets.toSeq(0).publicImage, 0L, Role.Hub.toString, "role"),
          ProfileBox(wallet.secrets.toSeq(1).publicImage, 0L, Role.Producer.toString, "role"),
          ProfileBox(wallet.secrets.toSeq(2).publicImage, 0L, Role.Investor.toString, "role")
        )
        val boxSC = BifrostStateChanges(Set(), profileBoxes.toSet, System.currentTimeMillis())

        state.applyChanges(boxSC, Ints.toByteArray(4)).get
      }
    }


    "Get the role after declaration" in {
      val requestBody = ByteString("""
       |{
       |  "jsonrpc": "2.0",
       |  "id": "16",
       |  "method": "getRole",
       |  "params": [{
       |      "publicKey": "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ"
       |  }, {
       |      "publicKey": "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU"
       |  }, {
       |      "publicKey": "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb"
       |  }]
       |}
       |""".stripMargin)
      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asArray.isDefined shouldEqual true
        (res \\ "error").isEmpty shouldEqual true
        val jsonArray = (res \\ "result").head.asArray.get
        (jsonArray(0) \\ "proposition").head.asString.get shouldEqual "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ"
        (jsonArray(0) \\ "value").head.asString.get shouldEqual "investor"
        (jsonArray(1) \\ "proposition").head.asString.get shouldEqual "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU"
        (jsonArray(1) \\ "value").head.asString.get shouldEqual "hub"
        (jsonArray(2) \\ "proposition").head.asString.get shouldEqual "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb"
        (jsonArray(2) \\ "value").head.asString.get shouldEqual "producer"
      }
    }

    val contractEffectiveTime = System.currentTimeMillis() + 100000L
    val contractExpirationTime = System.currentTimeMillis() + 200000000L
    val polyBoxes = view().vault.boxes().filter(_.box.isInstanceOf[PolyBox])
    val contractBodyTemplate = s"""
      {
        "jsonrpc": "2.0",
        "id": "16",
        "method": "getContractSignature",
        "params": [{
          "signingPublicKey": "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
          "agreement": {
            "assetCode": "WHEAT",
            "terms": {
              "pledge": 8000,
      			  "xrate": 1.564,
              "share": {
                "functionType": "PiecewiseLinearMultiple",
                "points": [[1.34, [0.25, 0.25, 0.5]]]
              },
              "fulfilment" : {
                "functionType" : "PiecewiseLinearSingle",
                "points" : [[${contractExpirationTime}, 1.00]]
              }
            },
            "contractEffectiveTime": ${contractEffectiveTime},
            "contractExpirationTime": ${contractExpirationTime}
          },
          "parties": {
            "investor": "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
            "hub": "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU",
            "producer": "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb"
          },
          "signatures": {
            "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ": "",
            "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU": "",
            "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb": ""
          },
          "preFeeBoxes": {
            "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ": [[${polyBoxes.head.box.nonce}, ${polyBoxes.head.box.value}]],
            "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU": [],
            "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb": []
          },
          "fees": {
            "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ": 500,
            "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU": 0,
            "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb": 0
          },
          "timestamp": ${System.currentTimeMillis()}
        }]
      }
      """
    var investorSig = ""; var hubSig = ""; var producerSig = ""

    "Get ContractCreation Signature" in {
      val requestBody = ByteString(contractBodyTemplate.stripMargin)
      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asObject.isDefined shouldEqual true
        investorSig = ((res \\ "result").head.asJson \\ "signature").head.asString.get
        investorSig.nonEmpty shouldEqual true
        (res \\ "error").isEmpty shouldEqual true
      }
      // Getting Producer and Hub signatures
      val paramsJson: Json = parse(contractBodyTemplate).getOrElse(Json.Null)
      val cursor: HCursor = paramsJson.hcursor
      val hubJson: Json = cursor.downField("params").downArray.downField("signingPublicKey")
        .withFocus(_.mapString(t => "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU")).top.get
      val producerJson: Json = cursor.downField("params").downArray.downField("signingPublicKey")
        .withFocus(_.mapString(t => "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb")).top.get

      httpPOST(ByteString(hubJson.toString)) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asObject.isDefined shouldEqual true
        hubSig = ((res \\ "result").head.asJson \\ "signature").head.asString.get
      }
      httpPOST(ByteString(producerJson.toString)) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asObject.isDefined shouldEqual true
        producerSig = ((res \\ "result").head.asJson \\ "signature").head.asString.get
      }
      println(s"Investor: ${investorSig}, Producer: ${producerSig}, Hub: ${hubSig}")
    }

    var contractBox = None: Option[ContractBox]
    var hubFeeBox = None: Option[PolyBox]; var producerFeeBox = None: Option[PolyBox]; var investorFeeBox = None: Option[PolyBox]
    "Create the Contract" in {
      val requestBodyJson = parse(contractBodyTemplate).getOrElse(Json.Null)
      val cursor: HCursor = requestBodyJson.hcursor
      val requestJson = cursor.downField("method")
        .withFocus(_.mapString(t => "createContract")).up
        .downField("params").downArray.downField("signatures").downField("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ")
        .withFocus(_.mapString(t => investorSig)).up
        .downField("F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU")
        .withFocus(_.mapString(t => hubSig)).up
        .downField("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb")
        .withFocus(_.mapString(t => producerSig)).top.get

      httpPOST(ByteString(requestJson.toString)) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asObject.isDefined shouldEqual true
        val txHash = ((res \\ "result").head.asObject.get.asJson \\ "transactionHash").head.asString.get
        view().pool.take(5).toList.size shouldEqual 4

        // manually add contractBox to state
        val txInstance = view().pool.getById(Base58.decode(txHash).get).get
        txInstance.newBoxes.foreach(b => b match {
          case b: ContractBox => contractBox = Some(b)
          case p: PolyBox =>
            if (p.proposition.pubKeyBytes sameElements Base58.decode("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb").get) {
              producerFeeBox = Some(p)
            } else if (p.proposition.pubKeyBytes sameElements Base58.decode("F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU").get) {
              hubFeeBox = Some(p)
            } else if (p.proposition.pubKeyBytes sameElements Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get) {
              investorFeeBox = Some(p)
            }
        })
        val boxSC = BifrostStateChanges(txInstance.boxIdsToOpen.toSet, txInstance.newBoxes.toSet, System.currentTimeMillis())

        view().state.applyChanges(boxSC, Ints.toByteArray(5)).get
        view().pool.remove(txInstance)
        view().pool.take(5).toList.size shouldEqual 3
      }
    }

    "Execute Deliver Method" in {
      val requestBody = s"""
          |{
          |  "jsonrpc": "2.0",
          |  "id": "19",
          |  "method": "executeContractMethod",
          |  "params": [{
          |    "signingPublicKey": "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb",
          |    "contractBox": ${contractBox.get.json},
          |    "methodName": "deliver",
          |    "parties": {
          |	     "producer": "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb"
          |	   },
          |	   "signatures": {
          |	     "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb": ""
          |	   },
          |	   "methodParams": {
          |      "quantity": 9000
          |	   },
          |	   "preFeeBoxes": {
          |	     "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb" : []
          |	   },
          |	   "fees" : {
          |       "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb" : 0
          |    },
          |    "timestamp": ${contractEffectiveTime + 1}
          |  }]
          |}
        """.stripMargin
      httpPOST(ByteString(requestBody)) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asObject.isDefined shouldEqual true
        val txHash = ((res \\ "result").head.asObject.get.asJson \\ "transactionHash").head.asString.get
        val txInstance = view().pool.getById(Base58.decode(txHash).get).get
        txInstance.newBoxes.foreach(b => b match {
          case b: ContractBox => contractBox = Some(b)
          case _ =>
        })
        val boxSC = BifrostStateChanges(txInstance.boxIdsToOpen.toSet, txInstance.newBoxes.toSet, System.currentTimeMillis())

        view().state.applyChanges(boxSC, Ints.toByteArray(6)).get
        view().pool.remove(txInstance)
        view().pool.take(5).toList.size shouldEqual 3
        Base58.encode(contractBox.get.id) sameElements (((res \\ "result").head \\ "contractBox").head \\ "id").head.asString.get
      }
    }
  }

  override def afterAll(): Unit = {
    val path: Path = Path ("/tmp/scorex/test-data")
    Try(path.deleteRecursively())
  }
}

object ContractRPCSpec {
  val settingsFileName = "testSettings.json"
  lazy val settings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFileName)
  }

  val path: Path = Path ("/tmp/scorex/test-data")
  Try(path.deleteRecursively())
}