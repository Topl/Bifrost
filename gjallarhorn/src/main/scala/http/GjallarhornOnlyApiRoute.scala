package http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import crypto.AddressEncoder.NetworkPrefix
import io.circe.Json
import io.circe.syntax._
import keymanager.KeyManager.SignTx
import requests.ApiRoute
import settings.AppSettings

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

case class GjallarhornOnlyApiRoute (settings: AppSettings,
                                    keyManager: ActorRef)
                                   (implicit val context: ActorRefFactory)
  extends ApiRoute {

  val namespace: Namespace = WalletNamespace

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    //TODO: enable gjallarhorn to create raw transaction.
    //case (method, params, id) if method == s"${namespace.name}_createRawTransaction"
    // => createRawTransaction(params.head, id)

    case (method, params, id) if method == s"${namespace.name}_signTx" => signTx(params.head, id)
    //case (method, params, id) if method == s"${namespace.name}_changeNetwork" => changeNetwork(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_connectedToBifrost" => getConnection
  }

  /**
    * Signs a transaction.
    * @param params - includes the singing keys, prototype, and message.
    * @param id
    * @return
    */
  private def signTx(params: Json, id: String): Future[Json] = {
    val tx = (params \\ "rawTx").head
    val messageToSign = (params \\ "messageToSign").head
    (for {
      signingKeys <- (params \\ "signingKeys").head.as[List[String]]
    } yield {
      (keyManager ? SignTx(tx, signingKeys, messageToSign)).mapTo[Json]
    }) match {
      case Right(value) => value
      case Left(error) => throw new Exception(s"error parsing signing keys: $error")
    }
  }

/*  private def changeNetwork(params: Json, id: String): Future[Json] = {
    (for {
      newNetwork <- (params \\ "newNetwork").head.as[String]
    } yield {

    })
  }*/

  private def getConnection: Future[Json] = {
    var connection: Boolean = true
    if (settings.application.chainProvider == "") {
      connection = false
    }
    Future{Map("connectedToBifrost" -> connection).asJson}
  }

}
