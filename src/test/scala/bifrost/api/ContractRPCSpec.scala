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
import bifrost.transaction.box.{ArbitBox, ProfileBox}
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

  "Contract RPC" should {
    "return role or error" in {
      val jsonRequest = ByteString("""
        |{
        |  "jsonrpc": "2.0",
        |  "id": "16",
        |  "method": "getRole",
        |  "params": [{
        |      "publicKey": "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU"
        |  }]
        |}
        |""".stripMargin)
      val postRequest = httpPOST(jsonRequest)
      postRequest ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").head.asObject.isDefined shouldEqual true
        (res \\ "result").isEmpty shouldEqual true
      }
    }

    "Create a role" in {
      val jsonRequest = ByteString("""
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
      val postRequest = httpPOST(jsonRequest)
      postRequest ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "result").head.asArray.isDefined shouldEqual true
        (res \\ "error").isEmpty shouldEqual true

        implicit val timeout = Timeout(5.seconds)
        val testing = (nodeViewHolderRef ? GetCurrentView)
          .mapTo[CurrentView[BifrostHistory, BifrostState, BWallet, BifrostMemPool]]
          .map { view: CurrentView[BifrostHistory, BifrostState, BWallet, BifrostMemPool] =>
            val state = view.state
            val wallet = view.vault
            val profileBoxes = Seq(
              ProfileBox(wallet.secrets.toSeq(0).publicImage, 0L, Role.Hub.toString, "role"),
              ProfileBox(wallet.secrets.toSeq(1).publicImage, 0L, Role.Producer.toString, "role"),
              ProfileBox(wallet.secrets.toSeq(2).publicImage, 0L, Role.Investor.toString, "role")
            )
            val boxSC = BifrostStateChanges(Set(), profileBoxes.toSet, System.currentTimeMillis())

            state.applyChanges(boxSC, Ints.toByteArray(4)).get
          }
        Await.result(testing, 5 seconds)
      }
    }

    "Get the role after declaration" in {
      val jsonRequest = ByteString("""
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
      val postRequest = httpPOST(jsonRequest)
      postRequest ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        println(res)
        (res \\ "result").head.asArray.isDefined shouldEqual true
        (res \\ "error").isEmpty shouldEqual true
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