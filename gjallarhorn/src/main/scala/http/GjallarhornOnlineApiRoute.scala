package http

import akka.actor.{ActorNotFound, ActorRef, ActorRefFactory, ActorSystem, PoisonPill, Props}
import akka.pattern.ask
import attestation.Address
import attestation.AddressEncoder.NetworkPrefix
import crypto.AssetCode
import io.circe.Json
import io.circe.syntax._
import keymanager.KeyManager._
import modifier.AssetValue
import requests.{ApiRoute, Requests, RequestsManager}
import settings.{ApplicationSettings, RPCApiSettings}
import utils.Logging
import wallet.WalletManager.{ConnectToBifrost, DisconnectFromBifrost, GetConnection}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

/**
 * Class route for managing online requests (bifrost must be running)
 * @param settings - API settings for ApiRoute
 * @param keyManager - actor reference for the KeyManager
 * @param walletManager - actor reference for the walletManager
 * @param requests - instance of requests class
 * @param context - ActorRef context
 * @param system - ActorSystem used to select and create actors
 */
case class GjallarhornOnlineApiRoute(
  settings:             RPCApiSettings,
  applicationSettings:  ApplicationSettings,
  keyManager:           ActorRef,
  walletManager:        ActorRef,
  requests:             Requests
)(implicit val context: ActorRefFactory, system: ActorSystem)
    extends ApiRoute
    with Logging {

  //Requests manager used to send requests to Bifrost
  private var requestsManager: Option[ActorRef] = None

  // Establish the expected network prefix for addresses
  implicit val networkPrefix: NetworkPrefix = _root_.keymanager.networkPrefix

  //The namespace for the endpoints defined in handlers
  val namespace: Namespace = OnlineWalletNamespace

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    case (method, params, id) if method == s"${namespace.name}_connectToBifrost"      => connectToBifrost(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_disconnectFromBifrost" => disconnectFromBifrost(id)
    case (method, params, id) if method == s"${namespace.name}_getConnection"         => getConnection(id)

    case (method, params, id) if method == s"${namespace.name}_createTransaction" => createTransaction(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_broadcastTx"       => broadcastTx(params.head, id)
  }

  /**
   * #### Summary
   * Connect To Bifrost
   *
   * #### Description
   * Attempts to connect to Bifrost using the currentChainProvider from settings.
   * ---
   * #### Params
   *
   * | Fields             | Data type | Required / Optional | Description |
   * | ------------------ | --------- | ------------------- | ----------- |
   * | --None specified-- |           |                     |             |
   *
   * @param params input parameters as specified above
   * @param id     request identifier
   * @return - either connectedToBifrost -> true or error message if unable to connect to the given chain provider.
   */
  private def connectToBifrost(params: Json, id: String): Future[Json] =
    (walletManager ? GetConnection).mapTo[Option[ActorRef]].map {
      case Some(actor) => throw new Exception("Already connected!")
      case None        => tryToConnect(params)
    }

  def tryToConnect(params: Json): Json = {
    val chainProvider = applicationSettings.defaultChainProviders.get(applicationSettings.currentChainProvider) match {
      case Some(cp) => cp.chainProvider
      case None =>
        throw new Exception(
          s"The current chain provider: ${applicationSettings.currentChainProvider} " +
          s"does not map to a chain provider in the list of chain providers."
        )
    }
    try {
      log.info("gjallarhorn attempting to run in online mode. Trying to connect to Bifrost...")
      val bifrost = Await.result(
        system
          .actorSelection(s"akka://$chainProvider/user/walletConnectionHandler")
          .resolveOne(),
        10.seconds
      )
      log.info(
        s"${Console.MAGENTA} Bifrst actor ref was found: $bifrost ${Console.RESET}. " +
        s"Now running in online mode."
      )
      setUpOnlineMode(bifrost)
    } catch {
      case e: ActorNotFound =>
        log.error(s"${Console.MAGENTA} bifrost actor ref not found at: akka://$chainProvider.${Console.RESET}")
        throw new Exception(s"could not connect to chain provider: $chainProvider. $e")
    }
  }

  /**
   * Helper function when connecting to Bifrost
   * Sends the bifrost actor ref to the walletManager and sets up the RequestsManager actor ref
   * @param bifrost - the actor ref for Bifrost
   * @return - Json(connectedToBifrost -> true)
   */
  def setUpOnlineMode(bifrost: ActorRef): Json = {
    val networkName = applicationSettings.defaultChainProviders.get(applicationSettings.currentChainProvider) match {
      case Some(cp) => cp.networkName
      case None =>
        throw new Exception(
          "The current chain provider name " +
          "does not match any of the keys in the default chain providers map."
        )
    }
    walletManager ! ConnectToBifrost(bifrost, networkName)

    val requestsManagerRef: ActorRef = system.actorOf(Props(new RequestsManager(bifrost)), name = "RequestsManager")
    requestsManager = Some(requestsManagerRef)
    requests.switchOnlineStatus(requestsManager)
    Map("connectedToBifrost" -> true).asJson
  }

  /**
   * #### Summary
   * Disconnect From Bifrost
   *
   * #### Description
   * Disconnects from Bifrost and kills the requestsManager actor.
   * ---
   * #### Params
   * | Fields                  	| Data type 	| Required / Optional 	| Description                    	  |
   * | ------------------------	| ----------	| --------------------	| ----------------------------------|
   * | --None specified--        |           	|                     	|                                   |
   *
   * @param id     request identifier
   * @return - status message
   */
  private def disconnectFromBifrost(id: String): Future[Json] = {
    var responseMsg = "Disconnected!"

    //tell walletManager to disconnect from bifrost
    walletManager ! DisconnectFromBifrost

    //Set requestsManager to None
    requestsManager match {
      case Some(actor) =>
        requestsManager = None
        actor ! PoisonPill
      case None => responseMsg = "Already disconnected from Bifrost"
    }

    //tell requests that it's in offline mode
    requests.switchOnlineStatus(requestsManager)

    log.info(s"${Console.MAGENTA}Gjallarhorn has been disconnected from Bifrost.${Console.RESET}")
    Future(Map("status" -> responseMsg).asJson)
  }

  /**
   * #### Summary
   * Get Connection
   *
   * #### Description
   * Returns Gjallarhorn's current connectivity status (if currently connected to bifrost or not).
   * ---
   * #### Params
   * | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
   * | ------------------------	| ----------	| --------------------	| -----------------------------------------------------------------------	  |
   * | --None specified--       |           	|                     	|                                                                         |
   *
   * @param id     request identifier
   * @return - "connectedToBifrost" -> true or false
   */
  private def getConnection(id: String): Future[Json] =
    (walletManager ? GetConnection).mapTo[Option[ActorRef]].map {
      case Some(actor) => Map("connectedToBifrost" -> true).asJson
      case None        => Map("connectedToBifrost" -> false).asJson
    }

  /**
   * Function used to create expected json mapping for recipients: IndexedSeq[(Address, AssetValue)]
   * Specifically, given params with recipients, amount, issuer, and shortname, generates an AssetCode and
   * creates tuples for each recipient of the address and generated AssetValue
   * @param params - Json mapping that should include recipients, amount, issuer, shortname
   * @return json mapping of "recipients" to IndexedSeq[(Address, AssetValue)]
   */
  private def createAssetCode(params: Json): Json =
    (for {
      rcpts     <- (params \\ "recipients").head.as[IndexedSeq[Address]]
      quantity  <- (params \\ "amount").head.as[Long]
      issuer    <- (params \\ "issuer").head.as[Address]
      shortName <- (params \\ "shortName").head.as[String]
    } yield
    //TODO: what should assetCode version be?
    Try(AssetCode(1.toByte, issuer, shortName)) match {
      case Success(assetCode) =>
        val recipients: IndexedSeq[(Address, AssetValue)] =
          rcpts.map(addr => (addr, modifier.AssetValue(quantity, assetCode))).toIndexedSeq
        params.deepMerge(Map("recipients" -> recipients).asJson)
      case Failure(exception) =>
        throw new Exception(s"Unable to generate asset code: $exception")
    }) match {
      case Right(value) => value
      case Left(ex)     => throw new Exception(s"error parsing signing keys: $ex")
    }

  /**
   * #### Summary
   * Create Transaction
   *
   * #### Description
   * Either creates a raw transaction if offline (online = false) or creates, signs, and broadcasts a transaction if online = true
   * ---
   * #### Params
   *
   * | Fields | Data type | Required / Optional | Description |
   * | ---| ---	| --- | --- |
   * | method | String	| Required | The method to send request to Bifrost ("topl_raw[Asset, Arbit, or Poly]Transfer")|
   * | params | Json	| Required | The params for the method to send to Bifrost|
   * In the inner params:
   * | online | Boolean	| Required | Defines whether to send online transaction or create raw transaction offline.|
   * | sender | IndexedSeq[Address]	| Required | Array of addresses from which assets should be sent |
   *
   * @param params input parameters as specified above
   * @param id     request identifier
   * @return - a response after creating transaction.
   */
  private def createTransaction(params: Json, id: String): Future[Json] = {
    var response = Future("tx".asJson)
    for {
      method      <- (params \\ "method").head.as[String]
      innerParams <- (params \\ "params").head.asArray.get.head.as[Json]
      online      <- (innerParams \\ "online").head.as[Boolean]
      sender      <- (innerParams \\ "sender").head.as[IndexedSeq[Address]]
    } yield {
      var params = innerParams
      if (method == "topl_rawAssetTransfer") {
        params = createAssetCode(innerParams)
      }
      val tx = requests.transaction(method, params)
      if (online) {
        val txResponse = requests.sendRequest(tx)
        val rawTx = (txResponse \\ "rawTx").head
        val msgToSign = (txResponse \\ "messageToSign").head.asString.get
        val signedTx = Await.result(
          (keyManager ? SignTx(rawTx, sender, msgToSign))
            .mapTo[Json],
          10.seconds
        )
        response = Future((requests.broadcastTx(signedTx) \\ "result").head)
      } else {
        response = Future((requests.sendRequest(tx) \\ "result").head)
      }
    }
    response
  }

  /**
   * #### Summary
   * Broadcast transaction
   *
   * #### Description
   * Sends broadcast request to Bifrost to broadcast signed transaction.
   *
   * #### Notes
   *    - Currently only enabled for `AssetCreation` and `AssetTransfer` transactions
   *      ---
   *      #### Params
   *      | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	      |
   *      |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	      |
   *      | tx                  	    | object     	| Required            	| A full formatted transaction JSON object (prototype transaction + signatures) |
   *
   * @param params input parameters as specified above
   * @param id     request identifier
   * @return
   */
  private def broadcastTx(params: Json, id: String): Future[Json] =
    requestsManager match {
      case Some(actor) =>
        val signedTx = (params \\ "params").head.asArray.get.head
        Future((requests.broadcastTx(signedTx) \\ "result").head)
      case None => offlineMessage()
    }

  /**
   * API response for error handling - when request comes in that requires Bifrost but Gjallarhorn is offline.
   * @return - error msg
   */
  private def offlineMessage(): Future[Json] = {
    val msg = "cannot send request because you are offline mode " +
      "or the chain provider provided was incorrect."
    throw new Exception(msg)
  }

}
