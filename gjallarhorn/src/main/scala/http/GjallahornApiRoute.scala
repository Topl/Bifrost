package http

import akka.actor.{ActorRefFactory, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import requests.{ApiRoute, Requests}
import io.circe.Json
import keymanager.KeyManager

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class GjallahornApiRoute(implicit val context: ActorRefFactory) extends ApiRoute {
//  //Necessary Akka Actor Components
  implicit val actorsystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
////--------------------------------------------------------------------------------------
//  //PROSPECTIVE ACTOR FUNCTIONALITY3
//
//  //DOMAIN: KeyManagerActor
//    //[???] Is it possible to extend strict logging for actors?
//  object KeyManagerActor {
//    case class lockKeyFile(publicKeyString: String, password: String)
//    case class unlockKeyFile(publicKeyString: String, password: String)
//  }
//
//  //Essential for unique instantiation of KeyManagerActor
//  object KeyManagerActorRef {
//    def apply(var secrets: Set[PrivateKey25519], defaultKeyDir: String)(implicit actorsystem: ActorSystem, ec: ExecutionContext): ActorRef = actorsystem.actorOf(props(secrets, defaultKeyDir))
//    def props(var secrets: Set[PrivateKey25519], defaultKeyDir: String)(implicit ec: ExecutionContext): Props = Props(new KeyManagerActor(secrets, defaultKeyDir))
//  }
//
//  class KeyManagerActor {
//    //If stateful actor, list variables here
//
//    override def receive: Receive = {
//      case lockKeyFile(publicKeyString, password) => ???
//      case unlockKeyFile(publicKeyString, password) => ???
//    }
//  }

//--------------------------------------------------------------------------------------
  val r = new Requests
  override val route: Route = pathPrefix("gjallarhorn") {basicRoute(handlers) }

  Http().newServerAt("localhost", 9086).bind(route)

  def handlers(method: String, params: Vector[Json], id: String): Future[Json] =
    method match {
      case "transaction" => createAssetsPrototype(params.head, id)
      case "signTx" => signTx(params.head, id)
      case "broadcastTx" => broadcastTx(params.head, id)
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
    // this is going to be sketchy... but there's no other way to get the keyManager instance...
    val defaultKeyDir = (params \\  "defaultKeyDir").head.asString.get
    val keyManager = KeyManager(Set(), defaultKeyDir)

    Future{r.signTx(tx, keyManager, props)}
  }

  private def broadcastTx(params: Json, id: String): Future[Json] = {
    Future{r.broadcastTx(params)}
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

