package bifrost.api

import java.util.concurrent.ConcurrentLinkedDeque

import org.scalatest.{Matchers, WordSpec}
import akka.http.scaladsl.model.{HttpEntity, HttpRequest, StatusCodes}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.server._
import Directives._
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import bifrost.BifrostNodeViewHolder
import bifrost.api.http.ContractApiRoute
import bifrost.forging.ForgingSettings
import io.circe
import scorex.core.settings.Settings
import io.circe._, io.circe.parser._
import io.circe.generic.auto._
import io.circe.syntax._

/**
  * Created by cykoz on 6/13/2017.
  */

trait RPCService {
  implicit val system:ActorSystem
  implicit val materializer:ActorMaterializer

  val settingsFileName = "settings.json"
  implicit lazy val settings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFileName)
  }

  val actorSystem = ActorSystem(settings.agentName)
  val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(classOf[BifrostNodeViewHolder], settings))
  val route = ContractApiRoute(settings, nodeViewHolderRef).route

}

class ContractRPCSpec extends WordSpec
  with Matchers
  with ScalatestRouteTest
  with RPCService {
  println(settings.toString)

  "Contract RPC" should {
    "return role or error" in {
      val jsonRequest = ByteString("""
        |{
        |    "jsonrpc": "2.0",
        |    "id": "16",
        |    "method": "getRole",
        |    "params": [{
        |        "publicKey": "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ"
        |    }, {
        |        "publicKey": "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU"
        |    }]
        |}
        |""".stripMargin)
      val postRequest = HttpRequest(
        HttpMethods.POST,
        uri = "/contract/",
        entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
      )
      postRequest ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").head.asObject.isDefined shouldEqual true
        (res \\ "result").isEmpty shouldEqual true
      }
    }
  }
}
