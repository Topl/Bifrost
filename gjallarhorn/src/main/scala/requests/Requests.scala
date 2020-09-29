package requests

import akka.actor.ActorSystem
import akka.http.scaladsl.{Http, HttpExt}
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.stream.ActorMaterializer
import akka.util.{ByteString, Timeout}
import keymanager.Keys
import crypto._
import io.circe.{Json, parser}
import io.circe.syntax._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.collection.mutable.{Map => MMap}
import scorex.crypto.encode.Base58
import settings.AppSettings
import shapeless.ops.product.ToTuple


class Requests (settings: AppSettings) {
  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val http: HttpExt = Http(actorSystem)

  val timeout: Timeout = Timeout(10.seconds)

  val requestPort: Int = settings.requestPort
  val requestAddress: String = settings.requestAddress

  //Generic Method for HTTP POST request
  def httpPOST(jsonRequest: ByteString, path: String): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = s"http://$requestAddress:$requestPort/$path/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  def requestResponseByteString(request: HttpRequest): Future[ByteString] = {
    val response = http.singleRequest(request)
    response.flatMap {
      case _@HttpResponse(StatusCodes.OK, _, entity, _) =>
        entity.dataBytes.runFold(ByteString.empty) { case (acc, b) => acc ++ b }
      case _ => sys.error("something wrong")
    }
  }

  def byteStringToJSON(data: Future[ByteString]): Json = {
    val parsedData: Future[Json] = data.map { x =>
      parser.parse(x.utf8String) match {
        case Right(parsed) => parsed
        case Left(e) => throw e.getCause
      }
    }
    Await.result(parsedData, 20 seconds)
  }

  def byteStringToJSON(data: ByteString): Json = {
    parser.parse(data.utf8String) match {
        case Right(parsed) => parsed
        case Left(e) => throw e.getCause
      }
  }

  def jsonToByteString(data: Json): ByteString = {
    val result = (data \\ "result").head
    val tx = (result \\ "formattedTx").head
    val params = Map(
      "tx" -> tx
    ).asJson
    val newJSON = Map(
      "jsonrpc" -> (data \\ "jsonrpc").head,
      "id" -> (data \\ "id").head,
      "method" -> "broadcastTx".asJson,
      "params" -> List(params).asJson
    ).asJson
    ByteString(newJSON.toString.getBytes())
  }

  def signTx(transaction: Json, keyManager: Keys, signingKeys: List[String]): Json = {
    val result = (transaction \\ "result").head
    val tx = (result \\ "formattedTx").head
    val messageToSign = (result \\ "messageToSign").head
    assert(signingKeys.contains((tx \\ "issuer").head.asString.get))

    var sigs: List[(String, String)] = signingKeys.map { pk =>
      val pubKey = PublicKey25519Proposition(Base58.decode(pk).get)
      val privKey = keyManager.secrets.find(sk => sk.publicKeyBytes sameElements pubKey.pubKeyBytes)

      privKey match {
        case Some(sk) => {
          val signature = Base58.encode(PrivateKey25519Companion.sign(sk, Base58.decode(messageToSign.asString.get).get).signature)
          (pk, signature)
        }
        case None => throw new NoSuchElementException
      }
    }

    val newTx = tx.deepMerge(Map(
      "signatures" -> sigs.toMap.asJson
    ).asJson)
    val newResult = Map("formattedTx"-> newTx).asJson
    Map(
      "jsonrpc" -> (transaction \\ "jsonrpc").head.asJson,
      "id" -> (transaction \\ "id").head.asJson,
      "result" -> newResult
    ).asJson
  }


  def transaction(params: Json): ByteString = {
    val method = (params \\ "method").head.asString.get
    val innerParams = (params \\ "params").head.asArray.get.head
    var requestBody: ByteString = ByteString.empty
    requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "2",
           |   "method": "$method",
           |   "params": [$innerParams]
           |}
       """.stripMargin)

    requestBody
  }

  def sendRequest(request: ByteString, path: String): Json  = {
    val sendTx = httpPOST(request, path)
    val data = requestResponseByteString(sendTx)
    byteStringToJSON(data)
  }

  def broadcastTx(signedTransaction: Json): Json = {
    val tx = jsonToByteString(signedTransaction)
    sendRequest(tx, "wallet")
  }

  def getBalances (publicKeys: Set[String]): Json = {
    var keysWithQuotes: Set[String] = Set.empty
    publicKeys.foreach(pk =>
      keysWithQuotes += s""""$pk""""
    )
    val keys: String = keysWithQuotes.mkString(", \n")
    val json = (
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "1",
         |   "method": "balances",
         |   "params": [
         |      {
         |        "publicKeys": [
         |            $keys
         |        ]
         |      }
         |   ]
         |}
       """
    )
    val requestBody = ByteString(json.stripMargin)
    sendRequest(requestBody, "wallet")
  }

/*
  def broadcastTx2(signedTransaction: Json): Json = {
    val tx = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "broadcastTx",
         |   "params": [$signedTransaction]
         |}
       """.stripMargin)
    sendRequest(tx, "wallet")
  }

  def parseTo(jsons: List[Json]): MMap[PublicKey25519Proposition, Long] = {
    val mapping: MMap[PublicKey25519Proposition, Long] = MMap.empty
    val toList: Array[String] = jsons.head.toString().stripPrefix("[").stripSuffix("]").split(",")
    val updatedList: Array[String] = new Array[String](toList.length)
    for(index <- 0 to toList.length-1) {
      if (index % 2 == 0) {
        System.out.println(toList(index).substring(2).trim)
        val pubKey = toList(index).substring(2).trim.stripPrefix("\"").stripSuffix("\"")
        updatedList(index) = pubKey
        System.out.println("pub key: " + pubKey)
      }else{
        val amount = toList(index).substring(2, toList(index).length-2).trim.stripPrefix("\"").stripSuffix("\"")
        updatedList(index) = amount
        System.out.println("amount: " + amount)
      }
    }
    mapping
  }


 def boxesToAdd(transaction: Json): MMap[String, MMap[String, Json]] = {
   val toAdd: MMap[String, MMap[String, Json]] = MMap.empty
   val boxes: MMap[String, Json] = MMap.empty
   val tx: Json = (transaction \\ "tx").head
   val newBoxes: List[Json] = tx \\ "newBoxes"
   if (newBoxes.nonEmpty) {
     val issuer = (tx \\ "issuer").head.toString().stripPrefix("\"").stripSuffix("\"")
     val toMap = parseTo(tx\\"to")
     System.out.println(toMap)
     val to = (tx \\ "to").head.asArray.head.head.toString().split(",")
     val pubKey = to.head.substring(2).trim
     val amount = to.tail.head
     val amountString = amount.substring(2, amount.length-2).trim.stripPrefix("\"").stripSuffix("\"")
     val publicKey = pubKey.stripPrefix("\"").stripSuffix("\"")
     newBoxes.map(id => {
       boxes.put(id.toString(),
         Map(
           "typeOfBox" -> "asset",
           "nonce" -> "0",
           "value" -> amountString,
           "issuer" -> issuer
         ).asJson)
     })
     toAdd.put(publicKey, boxes)
   }
   toAdd
  }

  def boxesToRemove(transaction: Json): List[(String, List[String])] = {
    var toRemove: List[(String, List[String])] = List.empty
    val tx: Json = (transaction \\ "tx").head
    val boxesToRemove: List[Json] = (tx \\ "boxesToRemove")
    if (boxesToRemove.nonEmpty) {
      val removeList: List[String] = boxesToRemove.map(id => id.toString())
      val pubKey = (tx \\ "to").head.toString()
      toRemove = toRemove :+ ((pubKey, removeList))
    }
    toRemove
  }*/

}


