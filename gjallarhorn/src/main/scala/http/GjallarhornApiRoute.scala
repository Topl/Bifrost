package http

import akka.actor.{ActorRef, ActorRefFactory, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.util.ByteString
import requests.{ApiRoute, Requests}
import io.circe.Json
import io.circe.syntax._
import keymanager.KeyManager._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object GjallarhornApiRoute {
  val r = new Requests
  def requestResponseByteString(request: HttpRequest): Future[ByteString] = {
    r.requestResponseByteString(request)
  }

  def byteStringToJSON(data: Future[ByteString]): Json = {
    r.byteStringToJSON(data)
  }
}

case class GjallarhornApiRoute(keyManager: ActorRef)(implicit val context: ActorRefFactory) extends ApiRoute {
//  //Necessary Akka Actor Components
  implicit val actorsystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val r = new Requests
  override val route: Route = pathPrefix("gjallarhorn") {basicRoute(handlers) }

  Http().newServerAt("localhost", 9086).bind(route)

  def handlers(method: String, params: Vector[Json], id: String): Future[Json] =
    method match {
      case "createAssetsPrototype" => createAssetsPrototype(params.head, id)
      case "signTx" => signTx(params.head, id)
      case "broadcastTx" => broadcastTx(params.head, id)
      case "listOpenKeyfiles" => listOpenKeyfiles(params.head, id)
      case "generateKeyfile" => generateKeyfile(params.head, id)
    }

  private def createAssetsPrototype(params: Json, id: String): Future[Json] = {
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
    Future{r.sendRequest(tx, "asset")}
  }

  private def signTx(params: Json, id: String): Future[Json] = {
    val props = (params \\ "signingKeys").head.asArray.get.map(k =>
     k.asString.get
    ).toList
    val tx = (params \\ "protoTx").head
    val messageToSign = (params \\ "messageToSign").head
    (keyManager ? SignTx(tx, props, messageToSign)).mapTo[String].map(_.asJson)
  }

  private def broadcastTx(params: Json, id: String): Future[Json] = {
    Future{r.broadcastTx(params)}
  }

  private def listOpenKeyfiles(params: Json, id: String): Future[Json] = {
    (keyManager ? GetOpenKeyfiles()).mapTo[Set[String]].map(_.asJson)
  }

  private def generateKeyfile(params: Json, id: String): Future[Json] = {
    val password = (params \\ "password").head.toString()
    (keyManager ? GenerateKeyFile(password)).mapTo[String].map(_.asJson)
  }
}

