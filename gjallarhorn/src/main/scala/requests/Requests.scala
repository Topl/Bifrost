package requests

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
 * Class that handles requests for bifrost
 * @param settings current App settings
 * @param keyManagerRef actor reference for the KeyManager
 * @param actorSystem current actor system
 */
class Requests(settings: AppSettings, keyManagerRef: ActorRef)(implicit val actorSystem: ActorSystem) {

  //akka extension for HTTP
  val http: HttpExt = Http(actorSystem)

  //optional actor reference to the RequestsManager
  private var requestsManager: Option[ActorRef] = None

  implicit val timeout: Timeout = Timeout(settings.rpcApi.timeout)
  val declaredAddress: String = settings.application.declaredAddress

  /**
   * Generic Method for HTTP POST request
   * @param jsonRequest json request as a ByteString
   * @param path the path for the http request, default to empty string
   * @return an HttpRequest for the given json request
   */
  def httpPOST(jsonRequest: ByteString, path: String = ""): HttpRequest = {
    val current = settings.application.currentChainProvider
    settings.application.defaultChainProviders.get(current) match {
      case Some(cp: HttpChainProvider) =>
        HttpRequest(
          HttpMethods.POST,
          uri = s"$declaredAddress/",
          entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
        ).withHeaders(RawHeader("x-api-key", cp.apiKey))
      case Some(_) =>
        throw new Exception(
          "The current chain provider is not an HttpChainProvider: " +
          s"${settings.application.currentChainProvider}"
        )
      case None =>
        throw new Exception(
          s"The current chain provider name: $current" +
          s"does not map to a chain provider in the list of chain providers."
        )
    }
  }

  /**
   * Gets the HTTP response from the http request and converts the response to a byte string
   * @param request the http request convert
   * @return the HTTP response as a future byte string
   */
  def requestResponseByteString(request: HttpRequest): Future[ByteString] = {
    val response = http.singleRequest(request)
    response.flatMap {
      case _ @HttpResponse(StatusCodes.OK, _, entity, _) =>
        entity.dataBytes.runFold(ByteString.empty) { case (acc, b) => acc ++ b }
      case error => sys.error(s"Error with the HTTP response: $error")
    }
  }

  /**
   * Converts future byte string to json
   * @param data the future byte string to convert
   * @return the future byte string in json form
   */
  def futureByteStringToJSON(data: Future[ByteString]): Json = {
    val parsedData: Future[Json] = data.map(x => byteStringToJSON(x))
    Await.result(parsedData, 20 seconds)
  }

  /**
   * Converts byte string to json
   * @param data the byte string to convert
   * @return the byte string in json form
   */
  def byteStringToJSON(data: ByteString): Json = {
    if (data.utf8String == "Provided API key is not correct") {
      val current = settings.application.currentChainProvider
      settings.application.defaultChainProviders.get(current) match {
        case Some(cp: HttpChainProvider) => throw new Exception(s"The api key: ${cp.apiKey} is not correct")
        case _ =>
          throw new Exception(
            "The provided API key is not correct and the current chain provider is not " +
            s"an HttpChainProvider: ${settings.application.currentChainProvider}"
          )
      }
    }
    parser.parse(data.utf8String) match {
      case Right(parsed) => parsed
      case Left(e)       => throw new Exception(s"Error parsing: ${data.utf8String}. $e")
    }
  }

  /**
   * Creates a correctly formatted json rpc response
   * @param request the original json request
   * @param response the json response from bifrost
   * @return correctly formatted rpc response
   */
  def createJsonResponse(request: Json, response: Json): Json = {
    val resultString = response
      .toString()
      .replace("\\", "")
      .replace("\"{", "{")
      .replace("}\"", "}")
    var resultJson: Json = response
    parse(resultString) match {
      case Left(f)          => throw f
      case Right(res: Json) => resultJson = res
    }
    Map(
      "jsonrpc" -> (request \\ "jsonrpc").head.asJson,
      "id"      -> (request \\ "id").head.asJson,
      "result"  -> resultJson
    ).asJson
  }

  /**
   * Signs a transaction
   * @param transaction the transaction to sign as json
   * @param signingKeys the addresses used to sign the transaction
   * @return a full signed transaction in json form
   */
  def signTx(transaction: Json, signingKeys: IndexedSeq[Address]): Json = {
    val result = (transaction \\ "result").head
    val tx = (result \\ "rawTx").head
    val messageToSign = (result \\ "messageToSign").head.asString.get
    val newResult = Await.result((keyManagerRef ? SignTx(tx, signingKeys, messageToSign)).mapTo[Json], 10.seconds)
    createJsonResponse(transaction, newResult)
  }

  /**
   * Creates a jsonrpc request
   * @param method the method name for the request
   * @param innerParams the params for the given method
   * @return the jsonrpc request as a ByteString
   */
  def transaction(method: String, innerParams: Json): ByteString = {
    var requestBody: ByteString = ByteString.empty
    requestBody = ByteString(s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "2",
           |   "method": "$method",
           |   "params": [$innerParams]
           |}
       """.stripMargin)

    requestBody
  }

  /**
   * Sends a request to bifrost (either through HTTP or Akka actor to WalletConnectionHandler)
   * @param request - the request to send as a byteString
   * @return the request response as json
   */
  def sendRequest(request: ByteString): Json =
    requestsManager match {
      case Some(actor) =>
        val current = settings.application.currentChainProvider
        settings.application.defaultChainProviders.get(current) match {
          case Some(_: HttpChainProvider) =>
            val sendTx = httpPOST(request)
            val data = requestResponseByteString(sendTx)
            futureByteStringToJSON(data)

          case Some(_: AkkaChainProvider) =>
            val req: Json = byteStringToJSON(request)
            val result = Await.result((actor ? BifrostRequest(req)).mapTo[String].map(_.asJson), 10.seconds)
            createJsonResponse(req, result)

          case None =>
            throw new Exception(
              s"No matching chain provider with name: " +
              s"${settings.application.currentChainProvider}"
            )
        }
      case None =>
        val msg = "cannot send request because you are offline mode " +
          "or the chain provider provided was incorrect."
        throw new Exception(msg)
    }

  /**
   * Broadcasts a transaction
   * @param signedTransaction the transaction to broadcast
   * @return the response from bifrost
   */
  def broadcastTx(signedTransaction: Json): Json =
    sendRequest(transaction("topl_broadcastTx", signedTransaction))

  /**
   * Retrieves the balances for the given keys from bifrost
   * @param addresses the addresses (as strings) to get balances for
   * @return the balances response from bifrost
   */
  def getBalances(addresses: Set[String]): Json = {
    val keysJson: Set[Json] = addresses.map(_.asJson)
    val params: Json = Map("addresses" -> keysJson.toList).asJson
    val requestBody = transaction("topl_balances", params)
    sendRequest(requestBody)
  }

  /**
   * Switches the current online status
   * Specifically, sets requestsManager to some actor ref (if connected to bifrost) or None (if offline)
   * @param requestsManagerRef the actor ref for the requests manager or None
   */
  def switchOnlineStatus(requestsManagerRef: Option[ActorRef]): Unit =
    requestsManager = requestsManagerRef

}
