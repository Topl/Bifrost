package http

import akka.actor.{ActorRef, ActorRefFactory, ActorSystem}
import akka.pattern.ask
import crypto.Address
import crypto.AddressEncoder.NetworkPrefix
import requests.{ApiRoute, Requests}
import io.circe.Json
import io.circe.syntax._
import keymanager.Bip39
import keymanager.KeyManager._
import requests.RequestsManager.BifrostRequest
import settings.AppSettings
import wallet.WalletManager.GetWallet

import scala.collection.mutable.{Map => MMap}
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

case class GjallarhornApiRoute(settings: AppSettings,
                               keyManager: ActorRef,
                               requestsManager: ActorRef,
                               walletManager: ActorRef,
                               requests: Requests)
                              (implicit val context: ActorRefFactory, actorSystem: ActorSystem, np: NetworkPrefix)
  extends ApiRoute {


  val namespace: Namespace = WalletNamespace

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    case (method, params, id) if method == s"${namespace.name}_createTransaction" => createTransaction(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_signTx" => signTx(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_listOpenKeyfiles"  => listOpenKeyfiles(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_broadcastTx"      => broadcastTx(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_generateKeyfile" => generateKeyfile(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_importKeyfile" => importKeyfile(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_unlockKeyfile" => unlockKeyfile(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_lockKeyfile" => lockKeyfile(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_getWalletBoxes" => getWalletBoxes()
    case (method, params, id) if method == s"${namespace.name}_balances" => balances(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_networkType" => Future{Map("networkPrefix" -> np).asJson}
    case (method, params, id) if method == s"${namespace.name}_generateMnemonic" => generateMnemonic(params.head)
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


  /**
    * Returns a list of the open key files.
    * @param params - no params needed.
    * @param id
    * @return - a list of the open key files once they are retrieved.
    */
  private def listOpenKeyfiles(params: Json, id: String): Future[Json] = {
    (keyManager ? GetOpenKeyfiles).mapTo[Set[Address]].map(_.asJson)
  }

  /**
    * Generates a key file.
    * @param params - contains the password for the key file.
    * @param id
    * @return
    */
  private def generateKeyfile(params: Json, id: String): Future[Json] = {
    val password = (params \\ "password").head.asString.get
    (keyManager ? GenerateKeyFile(password)).mapTo[Try[Address]].map {
      case Success(pk: Address) => Map("address" -> pk.asJson).asJson
      case Failure(ex) => throw new Error(s"An error occurred while creating a new keyfile. $ex")
    }
  }

  /**
    * Import a key file using a seed phrase.
    * @param params - should contain password, seedPhrase, seedPhraseLang
    * @param id
    * @return
    */
  private def importKeyfile(implicit params: Json, id: String): Future[Json] = {
    val password: String = (params \\ "password").head.asString.get
    val seedPhrase: String = (params \\ "seedPhrase").head.asString.get
    val seedPhraseLang: String = parseOptional("seedPhraseLang", "en")

    (keyManager ? ImportKeyfile(password, seedPhrase, seedPhraseLang)).mapTo[Try[Address]].map {
      case Success(pk: Address) => Map( "publicKey" -> pk.asJson).asJson
      case Failure(ex) => throw new Error(s"An error occurred while importing the seed phrase. $ex")
    }
  }

  /**
    * Unlocks a key file.
    * @param params - contains publicKey and password for the keyfile to unlock.
    * @param id
    * @return
    */
  private def unlockKeyfile(params: Json, id: String): Future[Json] = {
    val publicKey: String = (params \\ "publicKey").head.asString.get
    val password: String = (params \\ "password").head.asString.get

    (keyManager ? UnlockKeyFile(publicKey, password)).mapTo[Try[Unit]].map {
      case Success(_) => Map(publicKey -> "unlocked".asJson).asJson
      case Failure(ex) => throw new Error(s"An error occurred while trying to unlock the keyfile. $ex")
    }
  }

  /**
    * Locks a key file.
    * @param params - contains publicKey and password for the keyfile to lock.
    * @param id
    * @return
    */
  private def lockKeyfile(params: Json, id: String): Future[Json] = {
    val publicKey: String = (params \\ "publicKey").head.asString.get

    val password: String = (params \\ "password").head.asString.get
    (keyManager ? LockKeyFile(publicKey, password)).mapTo[Try[Unit]].map {
      case Success(_) => Map(publicKey -> "locked".asJson).asJson
      case Failure(ex) => throw new Error(s"An error occurred while trying to unlock the keyfile. $ex")
    }
  }

  private def balances(params: Json, id: String): Future[Json] = {
    val publicKeys: Set[String] = (params \\ "addresses").head.asArray.get.map(k => k.asString.get).toSet
    val requestBody = Map(
      "jsonrpc" -> "2.0".asJson,
      "id" -> "2".asJson,
      "method"-> (params \\ "method").head.asJson,
      "params" -> (params \\ "params").head.asJson
    ).asJson
    settings.application.communicationMode match {
      case "useTcp" => Future{requests.getBalances(publicKeys)}
      case "useAkka" => (requestsManager ? BifrostRequest(requestBody)).mapTo[String].map(_.asJson)
    }
  }

  private def generateMnemonic(params: Json): Future[Json] = {
    val lang = (params \\ "language").head.asString.get
    val phraseTranslator = Bip39.apply(lang)
    val phrase = phraseTranslator.uuidSeedPhrase(java.util.UUID.randomUUID.toString)._2
    Future{Map("mnemonicPhrase" -> phrase).asJson}
  }

  private def getWalletBoxes(): Future[Json] = {
    val walletResponse = Await.result((walletManager ? GetWallet).mapTo[MMap[String, MMap[String, Json]]], 10.seconds)
    Future{walletResponse.asJson}
  }

}

