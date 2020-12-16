package http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import crypto.Address
import keymanager.KeyManager._
import io.circe.Json
import io.circe.syntax._
import keymanager.{Bip39, networkPrefix}
import keymanager.KeyManager.{GenerateKeyFile, ImportKeyfile, LockKeyFile, UnlockKeyFile}
import requests.ApiRoute
import settings.AppSettings

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

case class KeyManagementApi(settings: AppSettings, keyManager: ActorRef)
                           (implicit val context: ActorRefFactory)
    extends ApiRoute {

  val namespace: Namespace = WalletNamespace

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    case (method, params, id) if method == s"${namespace.name}_generateKeyfile" => generateKeyfile(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_generateMnemonic" => generateMnemonic(params.head)
    case (method, params, id) if method == s"${namespace.name}_importKeyfile" => importKeyfile(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_unlockKeyfile" => unlockKeyfile(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_lockKeyfile" => lockKeyfile(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_listOpenKeyfiles" => listOpenKeyfiles(params.head, id)
  }

  /**
    * Returns a list of the open key files.
    *
    * @param params - no params needed.
    * @param id
    * @return - a list of the open key files once they are retrieved.
    */
  private def listOpenKeyfiles(params: Json, id: String): Future[Json] = {
    (keyManager ? GetOpenKeyfiles).mapTo[Set[Address]].map(_.asJson)
  }

  /**
    * Generates a key file.
    *
    * @param params - contains the password for the key file.
    * @param id
    * @return
    */
  private def generateKeyfile(params: Json, id: String): Future[Json] = {
    val password = (params \\ "password").head.asString.get
    val seedJson = params \\ "seed"
    var seed: Option[String] = None
    if (seedJson.nonEmpty) {
      seed = Some(seedJson.head.asString.get)
    }
    (keyManager ? GenerateKeyFile(password, seed)).mapTo[Try[Address]].map {
      case Success(pk: Address) => Map("address" -> pk.asJson).asJson
      case Failure(ex) => throw new Error(s"An error occurred while creating a new keyfile. $ex")
    }
  }

  /**
    * Import a key file using a seed phrase.
    *
    * @param params - should contain password, seedPhrase, seedPhraseLang
    * @param id
    * @return
    */
  private def importKeyfile(implicit params: Json, id: String): Future[Json] = {
    val password: String = (params \\ "password").head.asString.get
    val seedPhrase: String = (params \\ "seedPhrase").head.asString.get
    val seedPhraseLang: String = parseOptional("seedPhraseLang", "en")

    (keyManager ? ImportKeyfile(password, seedPhrase, seedPhraseLang)).mapTo[Try[Address]].map {
      case Success(pk: Address) => Map("publicKey" -> pk.asJson).asJson
      case Failure(ex) => throw new Error(s"An error occurred while importing the seed phrase. $ex")
    }
  }

  /**
    * Unlocks a key file.
    *
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
    *
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

  private def generateMnemonic(params: Json): Future[Json] = {
    val lang = (params \\ "language").head.asString.get
    val phraseTranslator = Bip39.apply(lang)
    val (seed, phrase) = phraseTranslator.uuidSeedPhrase(java.util.UUID.randomUUID.toString)
    Future{Map("seed" -> seed, "mnemonicPhrase" -> phrase).asJson}
  }

}
