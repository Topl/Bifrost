package http

import akka.actor.{ActorNotFound, ActorRef, ActorRefFactory, ActorSystem, PoisonPill, Props}
import akka.pattern.ask
import crypto.Address
import requests.{ApiRoute, Requests, RequestsManager}
import io.circe.Json
import io.circe.syntax._
import keymanager.KeyManager._
import requests.RequestsManager.BifrostRequest
import settings.{AppSettings, NetworkType}
import utils.Logging
import wallet.WalletManager
import wallet.WalletManager.{GetNetwork, GetWallet, GjallarhornStarted, GjallarhornStopped, KeyManagerReady}

import scala.collection.mutable.{Map => MMap}
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

case class GjallarhornBifrostApiRoute(settings: AppSettings,
                                      keyManager: ActorRef,
                                      requests: Requests)
                                     (implicit val context: ActorRefFactory, system: ActorSystem)
  extends ApiRoute with Logging {


  val namespace: Namespace = OnlineWalletNamespace

  private var requestsManager: Option[ActorRef] = None
  private var walletManager: Option[ActorRef] = None


  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    case (method, params, id) if method == s"${namespace.name}_connectToBifrost" => connectToBifrost(params.head)
    case (method, params, id) if method == s"${namespace.name}_disconnectFromBifrost" => disconnectFromBifrost
    case (method, params, id) if method == s"${namespace.name}_getConnection" => getConnection

    case (method, params, id) if method == s"${namespace.name}_createTransaction" => createTransaction(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_broadcastTx"      => broadcastTx(params.head, id)

    case (method, params, id) if method == s"${namespace.name}_balances" => balances(params.head)
    case (method, params, id) if method == s"${namespace.name}_getWalletBoxes" => getWalletBoxes
  }

  private def offlineMessage(): Future[Json] = {
    val msg = "cannot send request because you are offline mode " +
      "or the chain provider provided was incorrect."
    Future{Map("error" -> msg).asJson}
  }

  private def connectToBifrost(params: Json): Future[Json] = {
    val chainProvider = (params \\ "chainProvider").head.asString.get
    try {
      log.info("gjallarhorn attempting to run in online mode. Trying to connect to Bifrost...")
      val bifrost = Await.result(system.actorSelection(s"akka.tcp://$chainProvider/user/walletConnectionHandler")
        .resolveOne(), 10.seconds)
      log.info(s"${Console.MAGENTA} Bifrst actor ref was found: $bifrost ${Console.RESET}. " +
        s"Now running in online mode.")
      setUpOnlineMode(bifrost)
    } catch {
      case e: ActorNotFound =>
        log.error(s"${Console.MAGENTA} bifrost actor ref not found at: akka.tcp://$chainProvider." +
          s"Continuing to run in offline mode. ${Console.RESET}")
        Future{Map("error" -> s"could not connect to chain provider: $chainProvider. $e").asJson}
    }
  }

  def setUpOnlineMode(bifrost: ActorRef): Future[Json] = {
    //Set up wallet manager - handshake w Bifrost and get NetworkPrefix
    val walletManagerRef: ActorRef = system.actorOf(
      Props(new WalletManager(bifrost)), name = WalletManager.actorName)
    walletManager = Some(walletManagerRef)
    walletManagerRef ! GjallarhornStarted

    val bifrostResponse = Await.result((walletManagerRef ? GetNetwork).mapTo[String], 10.seconds)
    log.info(bifrostResponse)
    val networkName = bifrostResponse.split("Bifrost is running on").tail.head.replaceAll("\\s", "")
    val networkResponse = Await.result((keyManager ? ChangeNetwork(networkName)).mapTo[Json], 10.seconds)
    assert(NetworkType.fromString(networkName).get.netPrefix.toString ==
      (networkResponse \\ "newNetworkPrefix").head.asNumber.get.toString)
    walletManagerRef ! KeyManagerReady(keyManager)

    val requestsManagerRef: ActorRef = system.actorOf(Props(new RequestsManager(bifrost)), name = "RequestsManager")
    requestsManager = Some(requestsManagerRef)
    requests.switchOnlineStatus(requestsManager)
    Future{Map("connectedToBifrost" -> true).asJson}
  }

  private def disconnectFromBifrost: Future[Json] = {
    var responseMsg = "Disconnected!"
    walletManager match {
      case Some(walletManagerRef) =>
        walletManagerRef ! GjallarhornStopped
        walletManager = None
        walletManagerRef ! PoisonPill
      case None => responseMsg = "Already disconnected from Bifrost"
    }
    requestsManager match {
      case Some(actor) =>
        requestsManager = None
        actor ! PoisonPill
      case None => responseMsg = "Already disconnected from Bifrost"
    }

    requests.switchOnlineStatus(requestsManager)
    log.info(s"${Console.MAGENTA}Gjallarhorn has been disconnected from Bifrost.${Console.RESET}")
    Future{Map("status" -> responseMsg).asJson}
  }

  private def getConnection: Future[Json] = {
    walletManager match {
      case Some(actor) =>  Future{Map("connectedToBifrost" -> true).asJson}
      case None =>  Future{Map("connectedToBifrost" -> false).asJson}
    }
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
    * Broadcasts a transaction
    * @param params - tx: a full formatted transaction JSON object - prototype tx + signatures
    * @param id
    * @return
    */
  private def broadcastTx(params: Json, id: String): Future[Json] = {
    requestsManager match {
      case Some(actor) =>
        settings.application.communicationMode match {
          case "useTcp" => Future{requests.broadcastTx(params)}
          case "useAkka" => (actor ? BifrostRequest(params)).mapTo[String].map(_.asJson)
        }
      case None => offlineMessage()
    }
  }

  private def getWalletBoxes: Future[Json] = {
    walletManager match {
      case Some(actor) =>
        val walletResponse = Await.result((actor ? GetWallet).mapTo[MMap[String, MMap[String, Json]]], 10.seconds)
        Future{walletResponse.asJson}
      case None => offlineMessage()
    }

  }


  private def balances(params: Json): Future[Json] = {
    walletManager match {
      case Some(actor) =>
        val walletResponse: MMap[String, MMap[String, Json]] = Await.result((actor ? GetWallet)
          .mapTo[MMap[String, MMap[String, Json]]], 10.seconds)
        var publicKeys: Set[String] = walletResponse.keySet.toSet
        if ((params \\ "addresses").nonEmpty) {
          publicKeys = (params \\ "addresses").head.asArray.get.map(k => k.asString.get).toSet
        }
        val balances: MMap[Address, MMap[String, Long]] = MMap.empty
        publicKeys.foreach(addr => {
          val getBoxes: Option[MMap[String, Json]] = walletResponse.get(addr)
          var assets: MMap[String, Long] = MMap.empty
          getBoxes match {
            case Some(boxes) =>
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
            case None => null
          }
          balances.put(Address(addr), assets)
        })
        Future{balances.asJson}
      case None => offlineMessage()
    }

  }


}

