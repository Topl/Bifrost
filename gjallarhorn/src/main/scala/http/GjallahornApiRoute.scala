package http

//Akka Actor Dependencies
import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import akka.util.ByteString
import crypto.PrivateKey25519
import requests.{ApiRoute, Requests}
import io.circe.Json
import io.circe.syntax._
import keymanager.KeyManager
import scorex.crypto.encode.Base58

import scala.concurrent.Future

class GjallahornApiRoute extends ApiRoute {

  implicit val actorsystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
//--------------------------------------------------------------------------------------
  //PROSPECTIVE ACTOR FUNCTIONALITY

  //DOMAIN: KeyManagerActor
  object KeyManagerActor {
    def apply()
  }

  //Essential for unique instantiation of KeyManagerActor
  object KeyManagerActorRef {

  }

  class KeyManagerActor {

  }

//--------------------------------------------------------------------------------------
  val r = new Requests
  override val route: Route = pathPrefix("gjallarhorn") {basicRoute(handlers) }

  def handlers(method: String, params: Vector[Json], id: String): Future[Json] =
    method match {
      case "transaction" => createAssetsPrototype(params.head, id)
      case "signTx" => signTx(params.head, id)
      case "broadcastTx" => broadcastTx(params.head, id)
    }

  def createAssetsPrototype(params: Json, id: String): Future[Json] = {
    val issuer = (params \\ "issuer").head.asString.get
    val recipient = (params \\ "recipient").head.asString.get
    val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
    val assetCode: String =
      (params \\ "assetCode").head.asString.getOrElse("")
    val fee: Long =
      (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
    val data: String = (params \\ "data").headOption match {
      case Some(dataStr) => dataStr.asString.getOrElse("")
      case None          => ""
    }

    val tx = r.transaction("createAssetsPrototype", issuer, recipient, amount)
    r.sendRequest(tx, "asset")
  }

  def signTx(params: Json, id: String): Future[Json] = {
    val props = (params \\ "signingKeys").head.asArray.get.map(k =>
     k.asString.get
    ).toList
    val tx = (params \\ "protoTx").head
    // this is going to be sketchy... but there's no other way to get the keyManager instance...
    val secrets = (params \\ "secrets").head.asArray.get.map(k =>
      ???
    )
    val defaultKeyDir = (params \\  "defaultKeyDir").head.asString.get

    val keyManager = KeyManager(secrets, defaultKeyDir)

    r.signTx(tx, keyManager, props)
  }

  def broadcastTx(params: Json, id: String): Future[Json] = {
    r.broadcastTx(params)
  }



  /*
    transaction

    broadcast

    sign

    postJSON route

    API response.scala

    basicRoute --> check dev branch - in ApiRoute
  }
  routes

  need a handler (look at AssetAPI Route)
   */
}

