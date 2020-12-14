package http

import akka.actor.{ActorRef, ActorRefFactory, ActorSystem}
import akka.pattern.ask
import crypto.Address
import crypto.AddressEncoder.NetworkPrefix
import requests.{ApiRoute, Requests}
import io.circe.Json
import io.circe.syntax._
import keymanager.KeyManager._
import requests.RequestsManager.BifrostRequest
import settings.AppSettings
import wallet.WalletManager.GetWallet

import scala.collection.mutable.{Map => MMap}
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

case class GjallarhornApiRoute(settings: AppSettings,
                               keyManager: ActorRef,
                               requestsManager: ActorRef,
                               walletManager: ActorRef,
                               requests: Requests)
                              (implicit val context: ActorRefFactory, np: NetworkPrefix)
  extends ApiRoute {


  val namespace: Namespace = WalletNamespace

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    case (method, params, id) if method == s"${namespace.name}_createTransaction" => createTransaction(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_signTx" => signTx(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_broadcastTx"      => broadcastTx(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_networkType" => Future{Map("networkPrefix" -> np).asJson}

    case (method, params, id) if method == s"${namespace.name}_getWalletBoxes" => getWalletBoxes
    case (method, params, id) if method == s"${namespace.name}_balances" => balances(params.head, id)
  }

  /**
    * Creates a transaction.
    * @param params - contains the data for the transaction.
    * @param id
    * @return - a response after creating transaction.
    */
  private def createTransaction(params: Json, id: String): Future[Json] = {
    var response = Future{"tx".asJson}
    for {
      method <- (params \\ "method").head.as[String]
      innerParams <- (params \\ "params").head.asArray.get.head.as[Json]
      online <- (innerParams \\ "online").head.as[Boolean]
      sender <- (innerParams \\ "sender").head.as[IndexedSeq[Address]]
    } yield {
      val tx = requests.transaction(method, innerParams)
      if (online) {
        val txResponse = requests.sendRequest(tx)
        val rawTx = (txResponse \\ "rawTx").head
        val msgToSign = (txResponse \\ "messageToSign").head
        val signedTx = Await.result((keyManager ? SignTx(rawTx, List(sender.head.toString), msgToSign))
          .mapTo[Json], 10.seconds)
        response = Future{requests.broadcastTx(signedTx)}
      } else {
        response = Future {requests.sendRequest(tx)}
      }
    }
    response
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

  /**
    * Broadcasts a transaction
    * @param params - tx: a full formatted transaction JSON object - prototype tx + signatures
    * @param id
    * @return
    */
  private def broadcastTx(params: Json, id: String): Future[Json] = {
    settings.application.communicationMode match {
      case "useTcp" => Future{requests.broadcastTx(params)}
      case "useAkka" => (requestsManager ? BifrostRequest(params)).mapTo[String].map(_.asJson)
    }
  }

  private def balances(params: Json, id: String): Future[Json] = {
    val walletResponse: MMap[String, MMap[String, Json]] = Await.result((walletManager ? GetWallet).mapTo[MMap[String, MMap[String, Json]]], 10.seconds)
    var publicKeys: Set[String] = walletResponse.keySet.toSet
    if ((params \\ "addresses").nonEmpty) {
      publicKeys = (params \\ "addresses").head.asArray.get.map(k => k.asString.get).toSet
    }
    val balances: MMap[Address, MMap[String, Long]] = MMap.empty
    publicKeys.foreach(addr => {
      val getBoxes: Option[MMap[String, Json]] = walletResponse.get(addr)
      var assets: MMap[String, Long] = MMap.empty
      getBoxes match {
        case Some(boxes) => {
          var polyBalance: Long = 0
          var arbitBalance: Long = 0
          boxes.foreach(box => {
            for {
              boxType <- (box._2 \\ "type").head.as[String]
              value <- (box._2 \\ "value").head.as[Long]
            } yield {
              if (boxType == "ArbitBox") {
                arbitBalance = arbitBalance + value
              } else if (boxType == "PolyBox") {
                polyBalance = polyBalance + value
              }
            }
          })
          assets = MMap(
            "ArbitBox" -> arbitBalance,
            "PolyBox" -> polyBalance
          )
        }
        case None => null
      }
      balances.put(Address(addr), assets)
    })
    Future{balances.asJson}
  }

  private def getWalletBoxes: Future[Json] = {
    val walletResponse = Await.result((walletManager ? GetWallet).mapTo[MMap[String, MMap[String, Json]]], 10.seconds)
    Future{walletResponse.asJson}
  }

}

