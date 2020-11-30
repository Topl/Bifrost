package http

import akka.actor.{ActorRef, ActorRefFactory, ActorSystem}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import requests.{ApiRoute, Requests}
import io.circe.{Encoder, Json}
import io.circe.syntax._
import keymanager.KeyManager._
import requests.RequestsManager.{WalletRequest, AssetRequest}
import settings.AppSettings

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

case class GjallarhornApiRoute(settings: AppSettings,
                               keyManager: ActorRef,
                               requestsManager: ActorRef,
                               requests: Requests)
                              (implicit val context: ActorRefFactory,
                               implicit val actorSystem: ActorSystem) extends ApiRoute {

  override val route: Route = pathPrefix("gjallarhorn") {basicRoute(handlers) }

  /**
    * Handles the different methods that are called.
    * @param method - can be: createAssetsProtoType, signTX, broadcastTx, listOpenKeyfiles, generateKeyfile
    * @param params - parameters for the given method.
    * @param id - id for the transaction.
    * @return - returns a future response.
    */
  def handlers(method: String, params: Vector[Json], id: String): Future[Json] =
    method match {
        case "createTransaction" => createTransaction(params.head, id)
        case "signTx" => signTx(params.head, id)
        case "broadcastTx" => broadcastTx(params.head, id)
        case "listOpenKeyfiles" => listOpenKeyfiles(params.head, id)
        case "generateKeyfile" => generateKeyfile(params.head, id)
    }

  /**
    * Creates a transaction.
    * @param params - contains the data for the transaction.
    * @param id
    * @return - a response after creating transaction.
    */
  private def createTransaction(params: Json, id: String): Future[Json] = {
      val method: String = (params \\ "method").head.asString.get
      val innerParams: Json = (params \\ "params").head.asArray.get.head
      val tx = requests.transaction(method, innerParams)

      Future {
        requests.sendRequest(tx, "asset")
      }
  }

  /**
    * Signs a transaction.
    * @param params - includes the singing keys, prototype, and message.
    * @param id
    * @return
    */
  private def signTx(params: Json, id: String): Future[Json] = {
    val props = (params \\ "signingKeys").head.asArray.get.map(k =>
     k.asString.get
    ).toList
    val tx = (params \\ "protoTx").head
    val messageToSign = (params \\ "messageToSign").head
    (keyManager ? SignTx(tx, props, messageToSign)).mapTo[String].map(_.asJson)
  }

  /**
    * Broadcasts a transaction
    * @param params
    * @param id
    * @return
    */
  private def broadcastTx(params: Json, id: String): Future[Json] = {
    settings.useApiRoute match {
      case true => Future{requests.broadcastTx(params)}
      case false => (requestsManager ? WalletRequest(params)).mapTo[String].map(_.asJson)
    }
  }


  /**
    * Returns a list of the open key files.
    * @param params
    * @param id
    * @return - a list of the open key files once they are retrieved.
    */
  private def listOpenKeyfiles(params: Json, id: String): Future[Json] = {
    (keyManager ? GetOpenKeyfiles).mapTo[Set[String]].map(_.asJson)
  }

  /**
    * Generates a key file.
    * @param params - contains the password for the key file.
    * @param id
    * @return
    */
  private def generateKeyfile(params: Json, id: String): Future[Json] = {
    val password = (params \\ "password").head.toString()
    (keyManager ? GenerateKeyFile(password)).mapTo[String].map(_.asJson)
  }
}

