package bifrost.api

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpEntity, HttpRequest, _}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import bifrost.BifrostGenerators
import bifrost.api.http.ProgramApiRoute
import bifrost.forging.Forger
import bifrost.history.History
import bifrost.mempool.MemPool
import bifrost.modifier.box._
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import bifrost.network.message._
import bifrost.network.peer.PeerManager
import bifrost.network._
import bifrost.nodeView.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.nodeView.NodeViewHolder
import bifrost.state.{State, StateChanges}
import bifrost.wallet.Wallet
import com.google.common.primitives.Ints
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import org.scalatest.{Matchers, WordSpec}
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
  with BifrostGenerators {

  val path: Path = Path("/tmp/bifrost/test-data")
  Try(path.deleteRecursively())

  val actorSystem = ActorSystem(settings.agentName)
  val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new NodeViewHolder(settings)))
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

  val peerManagerRef: ActorRef = actorSystem.actorOf(Props(classOf[PeerManager], settings))

  val nProps = Props(classOf[NetworkController], settings, messagesHandler, upnp, peerManagerRef)
  val networkController: ActorRef = actorSystem.actorOf(nProps, "networkController")

  val forger: ActorRef = actorSystem.actorOf(Props(classOf[Forger], settings, nodeViewHolderRef))

  val localInterface: ActorRef = actorSystem.actorOf(
    Props(classOf[BifrostLocalInterface], nodeViewHolderRef, forger, settings)
  )

  val nodeViewSynchronizer: ActorRef = actorSystem.actorOf(
    Props(classOf[NodeViewSynchronizer],
          networkController,
          nodeViewHolderRef,
          localInterface,
          BifrostSyncInfoMessageSpec)
  )

  val route: Route = ProgramApiRoute(settings, nodeViewHolderRef, networkController).route

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/program/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  implicit val timeout: Timeout = Timeout(10.seconds)

  val publicKeys: Map[String, String] = Map(
    "investor" -> "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
    "producer" -> "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb",
    "hub" -> "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU"
  )

  private def view() = Await.result(
    (nodeViewHolderRef ? GetCurrentView)
      .mapTo[CurrentView[History, State, Wallet, MemPool]], 10.seconds)

  // Unlock Secrets
  val gw: Wallet = view().vault
  //gw.unlockKeyFile(publicKeys("investor"), "genesis")
  gw.unlockKeyFile(publicKeys("producer"), "genesis")
  gw.unlockKeyFile(publicKeys("hub"), "genesis")


  "Program RPC" should {

    val polyBoxes = view()
      .vault
      .boxes()
      .filter(_.box.isInstanceOf[PolyBox])

    val fees = Map(
      publicKeys("investor") -> 500,
      publicKeys("hub") -> 0,
      publicKeys("producer") -> 0
    )

    var executionBox: Option[ExecutionBox] = None


    def manuallyApplyChanges(res: Json, version: Int): Unit = {
      // Manually manipulate state
      val txHash = ((res \\ "result").head.asObject.get.asJson \\ "txHash").head.asString.get
      val txInstance: Transaction = view().pool.getById(Base58.decode(txHash).get).get
      txInstance.newBoxes.foreach {
        case b: ExecutionBox =>
          executionBox = Some(b)
        case _ =>
      }
      val boxSC = StateChanges(txInstance.boxIdsToOpen.toSet,
        txInstance.newBoxes.toSet,
        System.currentTimeMillis())

      view().state.applyChanges(boxSC, Ints.toByteArray(version)).get
      view().pool.remove(txInstance)
    }

    val program =
      s"""var x = 1
         |var y = 2
         |
         |add = function(a,b) {
         |return a + b
         |}""".stripMargin.asJson

    val programBodyTemplate =
      s"""
      {
        "jsonrpc": "2.0",
        "id": "16",
        "method": "getProgramSignature",
        "params": [{
         "signingPublicKey": "${publicKeys("investor")}",
          "program": $program,
          "readOnlyStateBoxes": [],
          "preInvestmentBoxes": [],
          "owner": "${publicKeys("investor")}",
          "signatures": ${Map(publicKeys("investor") -> "".asJson).asJson},
          "preFeeBoxes": {
            "${publicKeys("investor")}": [[${polyBoxes.head.box.nonce}, ${polyBoxes.head.box.value}]]
          },
          "fees": ${fees.asJson},
          "timestamp": ${System.currentTimeMillis},
          "data": ""
        }]
      }
      """

    var sig = ""

    "Get programCreation signature" in {
      val requestBody = ByteString(programBodyTemplate.stripMargin)
      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asObject.isDefined shouldEqual true
        sig = ((res \\ "result").head.asJson \\ "signature").head.asString.get
        sig.nonEmpty shouldEqual true
        (res \\ "error").isEmpty shouldEqual true
      }
    }

    "Create the Program" in {

      // TODO find and remove rogue AssetCreation tx
      val txs = view().pool.unconfirmed
      txs.map { tx =>
        view().pool.remove(tx._2)
      }

      val requestBodyJson = parse(programBodyTemplate).getOrElse(Json.Null)

      val cursor: HCursor = requestBodyJson.hcursor
      val requestJson = cursor.downField("method")
        .withFocus(_.mapString(_ => "createProgram")).up
        .downField("params").downArray.downField("signatures").downField(publicKeys("investor"))
        .withFocus(_.mapString(_ => sig)).top.get


      httpPOST(ByteString(requestJson.toString)) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asObject.isDefined shouldEqual true
        val txHash = ((res \\ "result").head.asObject.get.asJson \\ "txHash").head.asString.get
        //a new transaction in the mempool
        view().pool.take(1).toList.size shouldEqual 1

        // manually add program boxes to state
        manuallyApplyChanges(res, 8)

        view().pool.take(1).toList.size shouldEqual 0
      }
    }

    /*"Execute Program Method <changeStatus>" in {
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

        val state = root.value.executionBuilder.core.json.getOption(boxContent).get.as[BaseModuleWrapper].right.get.state
          .asString.get
        (parse(state).right.get \\ "status").head.asString.get shouldBe "in progress"
      }
    }*/

    /*"Get the program tx by bloom filter" in {
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