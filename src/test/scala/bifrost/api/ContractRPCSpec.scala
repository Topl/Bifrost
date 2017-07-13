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
import bifrost.network.PeerMessageManager
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView, GetSyncInfo}
import bifrost.scorexMod.GenericNodeViewSynchronizer.{GetLocalObjects, ResponseFromLocal}
import bifrost.state.{BifrostState, BifrostStateChanges}
import bifrost.transaction.{ContractCompletion, ProfileTransaction, Role}
import bifrost.transaction.box._
import bifrost.wallet.BWallet
import com.google.common.primitives.Ints
import io.circe
import scorex.core.settings.Settings
import io.circe._
import io.circe.parser._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.optics.JsonPath._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

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
  val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new BifrostNodeViewHolder(settings)))
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

  implicit val timeout = Timeout(10.seconds)

  val publicKeys = Map(
    "investor" -> "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
    "producer" -> "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb",
    "hub" -> "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU"
  )

  private def view() = Await.result((nodeViewHolderRef ? GetCurrentView)
    .mapTo[CurrentView[BifrostHistory, BifrostState, BWallet, BifrostMemPool]], 10 seconds)

  // Unlock Secrets
  val gw = view().vault
  // gw.unlockKeyFile(publicKeys("investor"), "genesis")
  gw.unlockKeyFile(publicKeys("producer"), "genesis")
  gw.unlockKeyFile(publicKeys("hub"), "genesis")

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
        (res \\ "error").head.asObject shouldBe defined
        (res \\ "result").isEmpty shouldBe true
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
        println(s"secrets in wallet, ${wallet.secrets}")
        val profileBoxes = Seq(
          ProfileBox(PublicKey25519Proposition(Base58.decode(publicKeys("hub")).get), 0L, Role.Hub.toString, "role"),
          ProfileBox(PublicKey25519Proposition(Base58.decode(publicKeys("producer")).get), 0L, Role.Producer.toString, "role"),
          ProfileBox(PublicKey25519Proposition(Base58.decode(publicKeys("investor")).get), 0L, Role.Investor.toString, "role")
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
                "points": [[0.0, [0.33333333, 0.33333333, 0.33333333]], [1.34, [0.25, 0.25, 0.5]]]
              },
              "fulfilment" : {
                "functionType" : "PiecewiseLinearSingle",
                "points" : [[0, 0.00], [${contractExpirationTime - contractEffectiveTime - 1}, 0.00],[${contractExpirationTime - contractEffectiveTime}, 1.00]]
              }
            },
            "contractEffectiveTime": ${contractEffectiveTime},
            "contractExpirationTime": ${contractExpirationTime}
          },
          "preInvestmentBoxes": [],
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

    def manuallyApplyChanges(res: Json, version: Int): Unit = {
      // Manually manipulate state
      val txHash = ((res \\ "result").head.asObject.get.asJson \\ "transactionHash").head.asString.get
      val txInstance = view().pool.getById(Base58.decode(txHash).get).get
      txInstance.newBoxes.foreach {
        case b: ContractBox => contractBox = Some(b)
        case _ =>
      }
      val boxSC = BifrostStateChanges(txInstance.boxIdsToOpen.toSet, txInstance.newBoxes.toSet, System.currentTimeMillis())

      view().state.applyChanges(boxSC, Ints.toByteArray(version)).get
      view().pool.remove(txInstance)
    }

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
        manuallyApplyChanges(res, 5)
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
          |    "contractBox": ${Base58.encode(contractBox.get.id).asJson},
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
        // Manually modify state
        manuallyApplyChanges(res, 6)
        // Assertions
        view().pool.take(5).toList.size shouldEqual 3
        val boxContent = ((res \\ "result").head \\ "contractBox").head
        Base58.encode(contractBox.get.id) shouldEqual (boxContent \\ "id").head.asString.get
        val _fulfillmentStatus = root.value.storage.currentFulfillment.json
        _fulfillmentStatus.getOption(boxContent).nonEmpty shouldEqual true
      }
    }

    "Execute confirmDelivery Method" in {
      val _deliveryId = root.value.storage.currentFulfillment.pendingDeliveries.each.id.string
      val deliveryId = _deliveryId.getAll(contractBox.get.json)
      val requestBody = s"""
         |{
         |  "jsonrpc": "2.0",
         |  "id": "19",
         |  "method": "executeContractMethod",
         |  "params": [{
         |    "signingPublicKey": "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU",
         |    "contractBox": ${Base58.encode(contractBox.get.id).asJson},
         |    "methodName": "confirmDelivery",
         |	   "methodParams": {
         |       "deliveryId": ${deliveryId.head.asJson}
         |	   },
         |    "parties": {
         |	     "hub": "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU"
         |	   },
         |	   "signatures": {
         |	     "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU": ""
         |	   },
         |	   "preFeeBoxes": {
         |	     "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU" : []
         |	   },
         |	   "fees" : {
         |       "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU" : 0
         |    },
         |    "timestamp": ${contractEffectiveTime + 10000L}
         |  }]
         |}
        """.stripMargin

      httpPOST(ByteString(requestBody)) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asObject.isDefined shouldEqual true
        manuallyApplyChanges(res, 7)
        // Assertions
        view().pool.take(5).toList.size shouldEqual 3
        val boxContent = ((res \\ "result").head \\ "contractBox").head
        Base58.encode(contractBox.get.id) shouldEqual (boxContent \\ "id").head.asString.get
        val _fulfillmentStatus = root.value.storage.currentFulfillment.deliveredQuantity.int
        _fulfillmentStatus.getOption(boxContent).get shouldEqual 9000
      }
    }

    def getEndorseCompletionBody(role: String, publicKey: String): String = {
      s"""
       |{
       |  "jsonrpc": "2.0",
       |  "id": "21",
       |  "method": "executeContractMethod",
       |  "params": [{
       |    "signingPublicKey": ${publicKey.asJson},
       |    "contractBox": ${Base58.encode(contractBox.get.id).asJson},
       |    "methodName": "endorseCompletion",
       |    "methodParams": {},
       |    "parties": {
       |	    ${role.asJson}: ${publicKey.asJson}
       |	  },
       |	  "signatures": {
       |	    ${publicKey.asJson}: ""
       |    },
       |    "preFeeBoxes": {
       |	    ${publicKey.asJson} : []
       |    },
       |    "fees" : {
       |      ${publicKey.asJson} : 0
       |    },
       |    "timestamp": ${contractEffectiveTime + 10000L}
       |  }]
       |}
      """.stripMargin
    }

    /**
      * We have to do this sequantially since we need the references of contractBox for each
      * request. If we map all three endorsement, we cannot guarantee if the contractBox has
      * been erased or not.
      */
    "Endorse Completions" in {
      // investor endorses 1st
      val requestBody = getEndorseCompletionBody("investor", publicKeys("investor"))
      httpPOST(ByteString(requestBody)) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asObject.isDefined shouldEqual true
        // modify state
        manuallyApplyChanges(res, 8)
      }
      // producer endorses 2nd
      val requestBody2 = getEndorseCompletionBody("producer", publicKeys("producer"))
      httpPOST(ByteString(requestBody2)) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asObject.isDefined shouldEqual true
        manuallyApplyChanges(res, 9)
      }
      // hub endorses last
      val requestBody3 = getEndorseCompletionBody("hub", publicKeys("hub"))
      httpPOST(ByteString(requestBody3)) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asObject.isDefined shouldEqual true
        println(res)
        // modify state
        manuallyApplyChanges(res, 10)
        // Assertions
        view().pool.take(5).toList.size shouldEqual 3
        val endorsements = root.result.contractBox.value.storage.endorsements.json.getOption(res)
        val endorseMap = endorsements.map(_.as[Map[String, String]].right.get).get
        endorseMap(publicKeys("investor")) shouldEqual endorseMap(publicKeys("producer"))
        endorseMap(publicKeys("investor")) shouldEqual endorseMap(publicKeys("hub"))
      }
    }

    "Get Contract Completion Signature" in {
      def requestBody(publicKey: String, role: String): String = {
        s"""
           |{
           |  "jsonrpc": "2.0",
           |  "id": "23",
           |  "method": "getCompletionSignature",
           |  "params": [{
           |    "signingPublicKey": ${publicKey.asJson},
           |    "contractBox": ${Base58.encode(contractBox.get.id).asJson},
           |    "reputationBoxes": [],
           |    "parties": ${publicKeys.asJson},
           |	  "signatures": {
           |	    ${publicKeys("investor").asJson}: "",
           |      ${publicKeys("producer").asJson}: "",
           |      ${publicKeys("hub").asJson}: ""
           |    },
           |    "preFeeBoxes": {
           |    },
           |    "fees" : {
           |    },
           |    "timestamp": ${contractEffectiveTime + 10000L}
           |  }]
           |}
        """.stripMargin
      }
      val requestBody1 = requestBody(publicKeys("investor"), "investor")
      httpPOST(ByteString(requestBody1)) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asObject.isDefined shouldEqual true
        investorSig = ((res \\ "result").head.asJson \\ "signature").head.asString.get
      }
      val requestBody2 = requestBody(publicKeys("producer"), "producer")
      httpPOST(ByteString(requestBody2)) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asObject.isDefined shouldEqual true
        producerSig = ((res \\ "result").head.asJson \\ "signature").head.asString.get
      }
      val requestBody3 = requestBody(publicKeys("hub"), "hub")
      httpPOST(ByteString(requestBody3)) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asObject.isDefined shouldEqual true
        hubSig = ((res \\ "result").head.asJson \\ "signature").head.asString.get
      }
    }

    var completionTx = None: Option[ContractCompletion]
    "Complete the Contract" in {
      val requestBody = s"""|{
      |  "jsonrpc" : "2.0",
      |  "id" : "23",
      |  "method": "completeContract",
      |  "params" : [{
      |  	 "contractBox": ${Base58.encode(contractBox.get.id).asJson},
      |  	 "reputationBoxes": [],
      |    "parties" : ${publicKeys.asJson},
      |    "signatures" : {
      |      ${publicKeys("investor").asJson} : ${investorSig.asJson},
      |      ${publicKeys("producer").asJson} : ${producerSig.asJson},
      |      ${publicKeys("hub").asJson} : ${hubSig.asJson}
      |    },
      |    "preFeeBoxes" : {
      |    },
      |    "fees" : {
      |    },
      |    "timestamp" : ${contractEffectiveTime + 10000L}
      |  }]
      |}
        """.stripMargin

      httpPOST(ByteString(requestBody)) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asObject.isDefined shouldEqual true
        val txHash = ((res \\ "result").head.asObject.get.asJson \\ "transactionHash").head.asString.get
        val txInstance = view().pool.getById(Base58.decode(txHash).get).get
        txInstance.boxIdsToOpen.head shouldEqual contractBox.get.id
        completionTx = Some(txInstance.asInstanceOf[ContractCompletion])
        val newBoxes = txInstance.newBoxes

        newBoxes.head.asInstanceOf[ReputationBox].value._1 shouldEqual 10.0
        newBoxes.head.asInstanceOf[ReputationBox].value._2 shouldEqual 7.0

        newBoxes.toList(1) shouldBe an[AssetBox]
        newBoxes.toList(1).asInstanceOf[AssetBox].value shouldEqual 325

        newBoxes.toList(2) shouldBe an[AssetBox]
        newBoxes.toList(2).asInstanceOf[AssetBox].value shouldEqual 325

        newBoxes.toList(3) shouldBe an[AssetBox]
        newBoxes.toList(3).asInstanceOf[AssetBox].value shouldEqual 8350

        val history = view().history
        val tempBlock = BifrostBlock(history.bestBlockId,
          System.currentTimeMillis(),
          ArbitBox(PublicKey25519Proposition(history.bestBlockId), 0L, 10000L),
          Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
          Seq(txInstance)
        )
        history.append(tempBlock)
      }
    }

    "Get the contract tx by bloom filter" in {
      val requestBody = s"""
        |{
        |  "jsonrpc" : "2.0",
        |  "id" : "23",
        |  "method": "filter",
        |  "params" : [${publicKeys("hub").asJson}]
        |}
        """.stripMargin

      httpPOST(ByteString(requestBody)) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        println(requestBody)
        println(res)
        (res \\ "result").head.asArray.get.nonEmpty shouldEqual true
        ((res \\ "result").head \\ "transactionHash").head.asString.get shouldEqual Base58.encode(completionTx.get.id)
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