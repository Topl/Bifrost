package http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import io.circe.Json
import io.circe.syntax._
import keymanager.KeyManager.{ChangeKeyfileDir, ChangeNetwork, SignTx}
import keymanager.networkPrefix
import requests.ApiRoute
import settings.AppSettings

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

case class GjallarhornOnlyApiRoute (settings: AppSettings,
                                    keyManagerRef: ActorRef)
                                   (implicit val context: ActorRefFactory)
  extends ApiRoute {

  val namespace: Namespace = WalletNamespace

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    //TODO: enable gjallarhorn to create raw transaction.
    //case (method, params, id) if method == s"${namespace.name}_createRawTransaction" =>
    // createRawTransaction(params.head, id)

    case (method, params, id) if method == s"${namespace.name}_signTx" => signTx(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_networkType" =>
      Future{Map("networkPrefix" -> networkPrefix).asJson}
    case (method, params, id) if method == s"${namespace.name}_changeNetwork" => changeNetwork(params.head, id)
  }

  /** #### Summary
    * Sign transaction
    *
    * #### Description
    * Signs a transaction - adds a signature to a raw transaction.
    * ---
    * #### Params
    *
    * | Fields | Data type | Required / Optional | Description |
    * | ---| ---	| --- | --- |
    * | rawTx | Json	| Required | The transaction to be signed. |
    * | signingKeys | List[String]	| Required | Keys used to create signatures to sign tx.|
    * | messageToSign | String | Required | The message to sign - in the form of an array of bytes.|
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return - transaction with signatures filled in.
    */
  private def signTx(params: Json, id: String): Future[Json] = {
    val tx = (params \\ "rawTx").head
    val messageToSign = (params \\ "messageToSign").head
    (for {
      signingKeys <- (params \\ "signingKeys").head.as[List[String]]
    } yield {
      (keyManagerRef ? SignTx(tx, signingKeys, messageToSign)).mapTo[Json]
    }) match {
      case Right(value) => value
      case Left(error) => throw new Exception(s"error parsing signing keys: $error")
    }
  }

  /** #### Summary
    * Change network
    *
    * #### Description
    * Changes the current network to the given network.
    * ---
    * #### Params
    *
    * | Fields | Data type | Required / Optional | Description |
    * | ---| ---	| --- | --- |
    * | newNetwork | String	| Required | the new network to switch to |
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return - "newNetworkPrefix" -> networkPrefix or an error message if the network name is not valid.
    */
  private def changeNetwork(params: Json, id: String): Future[Json] = {
    (for {
      newNetwork <- (params \\ "newNetwork").head.as[String]
    } yield {
      (keyManagerRef ? ChangeNetwork(newNetwork)).mapTo[Json]
    }) match {
      case Right(value) => value
      case Left(error) => throw new Exception (s"error parsing new network: $error")
    }
  }

}
