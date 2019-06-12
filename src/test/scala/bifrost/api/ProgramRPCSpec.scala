package bifrost.api

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpEntity, HttpRequest, _}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import bifrost.api.http.ProgramApiRoute
import bifrost.program.ExecutionBuilder
import bifrost.forging.Forger
import bifrost.history.{BifrostHistory, BifrostSyncInfoMessageSpec}
import bifrost.mempool.BifrostMemPool
import bifrost.network.BifrostNodeViewSynchronizer
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.state.{BifrostState, BifrostStateChanges}
import bifrost.transaction.box._
import bifrost.wallet.BWallet
import bifrost.{BifrostGenerators, BifrostLocalInterface, BifrostNodeViewHolder}
import com.google.common.primitives.Ints
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import bifrost.network.message._
import bifrost.network.peer.PeerManager
import bifrost.network.{NetworkController, UPnP}
import bifrost.transaction.bifrostTransaction.{BifrostTransaction, Role}
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.encode.Base58

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.io.Path
import scala.util.Try

/**
  * Created by cykoz on 6/13/2017.
  */

class ProgramRPCSpec extends WordSpec
  with Matchers
  with ScalatestRouteTest
  with BeforeAndAfterAll
  with BifrostGenerators {

  val path: Path = Path("/tmp/scorex/test-data")
  Try(path.deleteRecursively())

  val actorSystem = ActorSystem(settings.agentName)
  val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new BifrostNodeViewHolder(settings)))
  nodeViewHolderRef
  protected val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(BifrostSyncInfoMessageSpec)
  //p2p
  lazy val upnp = new UPnP(settings)

  private lazy val basicSpecs =
    Seq(
      GetPeersSpec,
      PeersSpec,
      InvSpec,
      RequestModifierSpec,
      ModifiersSpec
    )

  lazy val messagesHandler: MessageHandler = MessageHandler(basicSpecs ++ additionalMessageSpecs)

  val peerManagerRef = actorSystem.actorOf(Props(classOf[PeerManager], settings))

  val nProps = Props(classOf[NetworkController], settings, messagesHandler, upnp, peerManagerRef)
  val networkController = actorSystem.actorOf(nProps, "networkController")

  val forger: ActorRef = actorSystem.actorOf(Props(classOf[Forger], settings, nodeViewHolderRef))

  val localInterface: ActorRef = actorSystem.actorOf(
    Props(classOf[BifrostLocalInterface], nodeViewHolderRef, forger, settings)
  )

  val nodeViewSynchronizer: ActorRef = actorSystem.actorOf(
    Props(classOf[BifrostNodeViewSynchronizer],
          networkController,
          nodeViewHolderRef,
          localInterface,
          BifrostSyncInfoMessageSpec)
  )

  val route = ProgramApiRoute(settings, nodeViewHolderRef, networkController).route

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/program/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("api_key", "test_key"))
  }

  implicit val timeout = Timeout(10.seconds)

  val publicKeys = Map(
    "investor" -> "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
    "producer" -> "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb",
    "hub" -> "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU"
  )

  private def view() = Await.result(
    (nodeViewHolderRef ? GetCurrentView)
      .mapTo[CurrentView[BifrostHistory, BifrostState, BWallet, BifrostMemPool]], 10.seconds)

  // Unlock Secrets
  val gw: BWallet = view().vault
  //gw.unlockKeyFile(publicKeys("investor"), "genesis")
  gw.unlockKeyFile(publicKeys("producer"), "genesis")
  gw.unlockKeyFile(publicKeys("hub"), "genesis")

  "Program RPC" should {
    "return role or error" in {
      val requestBody = ByteString(
        s"""
           |{
           |  "jsonrpc": "2.0",
           |  "id": "1",
           |  "method": "getRole",
           |  "params": [{
           |      "publicKey": "${publicKeys("hub")}"
           |  }]
           |}
           |""".stripMargin)
      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").head.asObject.isDefined shouldBe true
        (res \\ "result").isEmpty shouldBe true
      }
    }

    "Create a role" in {
      val requestBody = ByteString(
        s"""
           |{
           |  "jsonrpc": "2.0",
           |  "id": "1",
           |  "method": "declareRole",
           |  "params": [{
           |        "publicKey": "${publicKeys("investor")}",
           |        "role": "investor"
           |    }, {
           |        "publicKey": "${publicKeys("hub")}",
           |        "role": "hub"
           |    }, {
           |        "publicKey": "${publicKeys("producer")}",
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
        //println(s"secrets in wallet, ${wallet.secrets}")
        val profileBoxes = Seq(
          ProfileBox(PublicKey25519Proposition(Base58.decode(publicKeys("investor")).get),
            0L,
            Role.Investor.toString,
            "role"),
          ProfileBox(PublicKey25519Proposition(Base58.decode(publicKeys("hub")).get), 0L, Role.Hub.toString, "role"),
          ProfileBox(PublicKey25519Proposition(Base58.decode(publicKeys("producer")).get),
                     0L,
                     Role.Producer.toString,
                     "role")
        )
        val boxSC = BifrostStateChanges(Set(), profileBoxes.toSet, System.currentTimeMillis())

        state.applyChanges(boxSC, Ints.toByteArray(7)).get
      }
    }

    "Get the role after declaration" in {
      val requestBody = ByteString(
        s"""
           |{
           |  "jsonrpc": "2.0",
           |  "id": "1",
           |  "method": "getRole",
           |  "params": [{
           |      "publicKey": "${publicKeys("investor")}"
           |  }, {
           |      "publicKey": "${publicKeys("hub")}"
           |  }, {
           |      "publicKey": "${publicKeys("producer")}"
           |  }]
           |}
           |""".stripMargin)
      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asArray.isDefined shouldEqual true
        (res \\ "error").isEmpty shouldEqual true
        val jsonArray = (res \\ "result").head.asArray.get
        (jsonArray(0) \\ "proposition").head.asString.get shouldEqual publicKeys("investor")
        (jsonArray(0) \\ "value").head.asString.get shouldEqual "investor"
        (jsonArray(1) \\ "proposition").head.asString.get shouldEqual publicKeys("hub")
        (jsonArray(1) \\ "value").head.asString.get shouldEqual "hub"
        (jsonArray(2) \\ "proposition").head.asString.get shouldEqual publicKeys("producer")
        (jsonArray(2) \\ "value").head.asString.get shouldEqual "producer"
      }
    }

    val programEffectiveTime = System.currentTimeMillis() + 100000L
    val programExpirationTime = System.currentTimeMillis() + 200000000L
    val polyBoxes = view()
      .vault
      .boxes()
      .filter(_.box.isInstanceOf[PolyBox])

    val agreement: ExecutionBuilder = sampleUntilNonEmpty(validAgreementGen(programEffectiveTime, programExpirationTime))

    val fees = Map(
      publicKeys("investor") -> 500,
      publicKeys("hub") -> 0,
      publicKeys("producer") -> 0
    )

    val programBodyTemplate =
      s"""
      {
        "jsonrpc": "2.0",
        "id": "1",
        "method": "getProgramSignature",
        "params": [{
          "signingPublicKey": "${publicKeys("investor")}",
          "agreement": ${agreement.asJson},
          "preInvestmentBoxes": [],
          "parties": ${publicKeys.map { case (k, v) => v -> k.asJson }.asJson},
          "signatures": ${publicKeys.map { case (k, v) => v -> "".asJson }.asJson},
          "preFeeBoxes": {
            "${publicKeys("investor")}": [[${polyBoxes.head.box.nonce}, ${polyBoxes.head.box.value}]],
            "${publicKeys("hub")}": [],
            "${publicKeys("producer")}": []
          },
          "fees": ${fees.asJson},
          "timestamp": ${System.currentTimeMillis},
          "data": ""
        }]
      }
      """
    var investorSig = ""
    var hubSig = ""
    var producerSig = ""

    /*"Get ProgramCreation Signature" in {
      val requestBody = ByteString(programBodyTemplate.stripMargin)
      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asObject.isDefined shouldEqual true
        investorSig = ((res \\ "result").head.asJson \\ "signature").head.asString.get
        investorSig.nonEmpty shouldEqual true
        (res \\ "error").isEmpty shouldEqual true
      }
      // Getting Producer and Hub signatures
      val paramsJson: Json = parse(programBodyTemplate).getOrElse(Json.Null)
      val cursor: HCursor = paramsJson.hcursor
      val hubJson: Json = cursor.downField("params").downArray.downField("signingPublicKey")
        .withFocus(_.mapString(_ => publicKeys("hub"))).top.get
      val producerJson: Json = cursor.downField("params").downArray.downField("signingPublicKey")
        .withFocus(_.mapString(_ => publicKeys("producer"))).top.get

      httpPOST(ByteString(hubJson.toString)) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asObject.isDefined shouldEqual true
        hubSig = ((res \\ "result").head.asJson \\ "signature").head.asString.get
        hubSig.nonEmpty shouldEqual true
        (res \\ "error").isEmpty shouldEqual true
      }
      httpPOST(ByteString(producerJson.toString)) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asObject.isDefined shouldEqual true
        producerSig = ((res \\ "result").head.asJson \\ "signature").head.asString.get
        hubSig.nonEmpty shouldEqual true
        (res \\ "error").isEmpty shouldEqual true
      }
      //println(s"Investor: $investorSig, Producer: $producerSig, Hub: $hubSig")
    }

    var programBox: Option[ProgramBox] = None
    var hubFeeBox: Option[PolyBox] = None
    var producerFeeBox: Option[PolyBox] = None
    var investorFeeBox: Option[PolyBox] = None

    def manuallyApplyChanges(res: Json, version: Int): Unit = {
      // Manually manipulate state
      val txHash = ((res \\ "result").head.asObject.get.asJson \\ "transactionHash").head.asString.get
      val txInstance: BifrostTransaction = view().pool.getById(Base58.decode(txHash).get).get
      txInstance.newBoxes.foreach {
        case b: ProgramBox => {
          programBox = Some(b)
        }
        case _ =>
      }
      val boxSC = BifrostStateChanges(txInstance.boxIdsToOpen.toSet,
                                      txInstance.newBoxes.toSet,
                                      System.currentTimeMillis())

      view().state.applyChanges(boxSC, Ints.toByteArray(version)).get
      view().pool.remove(txInstance)
    }*/

   /* "Create the Program" in {
      val requestBodyJson = parse(programBodyTemplate).getOrElse(Json.Null)

      val cursor: HCursor = requestBodyJson.hcursor
      val requestJson = cursor.downField("method")
        .withFocus(_.mapString(_ => "createProgram")).up
        .downField("params").downArray.downField("signatures").downField(publicKeys("investor"))
        .withFocus(_.mapString(_ => investorSig)).up
        .downField(publicKeys("hub"))
        .withFocus(_.mapString(_ => hubSig)).up
        .downField(publicKeys("producer"))
        .withFocus(_.mapString(_ => producerSig)).top.get


      httpPOST(ByteString(requestJson.toString)) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asObject.isDefined shouldEqual true
        val txHash = ((res \\ "result").head.asObject.get.asJson \\ "transactionHash").head.asString.get
        //Changed shouldEqual from 4 to 5 since AssetRPCSpec test was added, which creates
        //a new transaction in the mempool
        view().pool.take(5).toList.size shouldEqual 5

        // manually add programBox to state
        manuallyApplyChanges(res, 8)
        //Changed shouldEqual from 3 to 4 since AssetRPCSpec test was added, which creates
        //a new transaction in the mempool
        view().pool.take(5).toList.size shouldEqual 4
      }
    }

    "Execute Program Method <changeStatus>" in {
      val requestBody =
        s"""
           |{
           |  "jsonrpc": "2.0",
           |  "id": "19",
           |  "method": "executeProgramMethod",
           |  "params": [{
           |    "signingPublicKey": "${publicKeys("producer")}",
           |    "programBox": ${Base58.encode(programBox.get.id).asJson},
           |    "methodName": "changeStatus",
           |    "parties": {
           |	     "${publicKeys("producer")}" : "producer"
           |	   },
           |	   "signatures": {
           |	     "${publicKeys("producer")}": ""
           |	   },
           |	   "methodParams": {
           |      "newStatus": "in progress"
           |	   },
           |	   "preFeeBoxes": {
           |	     "${publicKeys("producer")}" : []
           |	   },
           |	   "fees" : {
           |       "${publicKeys("producer")}" : 0
           |    },
           |    "timestamp": ${programEffectiveTime + 1},
           |    "data": ""
           |  }]
           |}
        """.stripMargin

      httpPOST(ByteString(requestBody)) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asObject.isDefined shouldEqual true
        // Manually modify state
        manuallyApplyChanges(res, 9)
        // Assertions
        //Changed shouldEqual from 3 to 4 since AssetRPCSpec test was added, which creates
        //a new transaction in the mempool
        view().pool.take(5).toList.size shouldEqual 4
        val boxContent = ((res \\ "result").head \\ "programBox").head
        Base58.encode(programBox.get.id) shouldEqual (boxContent \\ "id").head.asString.get

        val state = root.value.agreement.core.json.getOption(boxContent).get.as[BaseModuleWrapper].right.get.state
          .asString.get
        (parse(state).right.get \\ "status").head.asString.get shouldBe "in progress"
      }
    }

    "Get the program tx by bloom filter" in {
      val requestBody =
        s"""
           |{
           |  "jsonrpc" : "2.0",
           |  "id" : "23",
           |  "method": "filter",
           |  "params" : [${publicKeys("hub").asJson}]
           |}
        """.stripMargin

      httpPOST(ByteString(requestBody)) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asArray.get.nonEmpty shouldEqual true
        ((res \\ "result").head \\ "transactionHash").head.asString.get shouldEqual Base58.encode(completionTx.get.id)
      }
    }*/

//    "Post a Proposal" in {
//      val tempProposal = ProducerProposal(
//        com.google.protobuf.ByteString.copyFrom("testProducer".getBytes),
//        ProposalDetails(assetCode = "assetCode", fundingNeeds = Some(ProposalDetails.Range(0, 1000)))
//      )
//      val requestBody =
//        s"""
//           |{
//           |  "jsonrpc" : "2.0",
//           |  "id" : "24",
//           |  "method": "postProposals",
//           |  "params" : [${JsonFormat.toJsonString(tempProposal)}]
//           |}
//        """.stripMargin
//      httpPOST(ByteString(requestBody)) ~> route ~> check {
//        val res = parse(responseAs[String]).right.get
//        (res \\ "result").head.asObject.isDefined shouldEqual true
//        val msgManager = Await.result((nodeViewHolderRef ? GetMessageManager).mapTo[MessageManager], 5.seconds)
//        msgManager.m.take(1).head.messageBytes.toByteArray sameElements tempProposal.toByteArray shouldBe true
//      }
//    }
//
//    "Retrieve Proposals" in {
//      val requestBody =
//        s"""
//           |{
//           |  "jsonrpc" : "2.0",
//           |  "id" : "24",
//           |  "method": "retrieveProposals",
//           |  "params" : [{
//           |    "limit": 10
//           |  }]
//           |}
//        """.stripMargin
//
//      httpPOST(ByteString(requestBody)) ~> route ~> check {
//        val res = parse(responseAs[String]).right.get
//        (res \\ "result").head.asObject.isDefined shouldEqual true
//        ((res \\ "result").head \\ "totalProposals").head.asNumber.get.toInt.get shouldEqual 1
//      }
//    }
  }
}