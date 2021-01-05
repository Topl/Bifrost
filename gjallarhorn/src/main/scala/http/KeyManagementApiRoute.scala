package http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import crypto.Address
import keymanager.KeyManager._
import io.circe.Json
import io.circe.syntax._
import keymanager.Bip39
import keymanager.KeyManager.{GenerateKeyFile, ImportKeyfile, LockKeyFile, UnlockKeyFile}
import requests.ApiRoute
import settings.AppSettings

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

case class KeyManagementApiRoute(settings: AppSettings, keyManager: ActorRef)
                                (implicit val context: ActorRefFactory)
    extends ApiRoute {

  val namespace: Namespace = WalletNamespace

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    case (method, params, id) if method == s"${namespace.name}_generateKeyfile" => generateKeyfile(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_generateMnemonic" => generateMnemonic(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_importKeyfile" => importKeyfile(params.head, id)

    case (method, params, id) if method == s"${namespace.name}_unlockKeyfile" => unlockKeyfile(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_lockKeyfile" => lockKeyfile(params.head, id)

    case (method, params, id) if method == s"${namespace.name}_listOpenKeyfiles" => listOpenKeyfiles(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_listAllKeyfiles" => listAllKeyfiles(params.head, id)
  }


  /** #### Summary
    * Return list of open keyfiles
    *
    * #### Type
    * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
    *
    * #### Description
    * Check which keyfiles are currently unlocked in your wallet. This method takes no input arguments.
    * ---
    * #### Params
    * | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    * | ------------------------	| ----------	| --------------------	| -----------------------------------------------------------------------	  |
    * | --None specified--       |           	|                     	|                                                                         |
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return - list of the addresses for the open key files
    */
  private def listOpenKeyfiles(params: Json, id: String): Future[Json] = {
    (keyManager ? GetOpenKeyfiles).mapTo[Set[Address]].map(_.asJson)
  }

  /** #### Summary
    * Return list of all keyfiles
    *
    * #### Description
    * Grabs all of the keyfiles in a given directory
    * ---
    * #### Params
    * | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    * | ------------------------	| ----------	| --------------------	| -----------------------------------------------------------------------	  |
    * | --None specified--       |           	|                     	|                                                                         |
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return - list of addresses for all of the key files
    */
  private def listAllKeyfiles(params: Json, id: String): Future[Json] = {
    (keyManager ? GetAllKeyfiles).mapTo[List[Address]].map(_.asJson)
  }

  /**
    * Generates a key file.
    *
    * @param params - contains the password for the key file.
    * @param id
    * @return - address for generated keyfile.
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

  /** #### Summary
    * Import key from mnemonic
    *
    * #### Description
    * Allows a user to import a 12, 15, 18, 21, or 24 word mnemonic (seed phrase) and generate an encrypted Keyfile
    * ---
    * #### Params
    * | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    * |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
    * | password                	| String    	| Required            	| String used to encrypt the private keyfile that is stored locally         |
    * | seedPhrase              	| String    	| Required            	| 12, 15, 18, 21, or 24 word mnemonic							         	                |
    * | seddPhraseLang         	| String    	| Optional            	| Defaults to 'en'. Valid options are ["zh-hans", "zh-hant", "en", "fr", "it", "ja", "ko", "es"] |
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return - returns the address for the imported key.
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

  /** #### Summary
    * Unlock keyfile
    *
    * #### Description
    * Unlock an encrypted keyfile which exists in your keyfile directory. This will add the secret key to wallet and allow signing of transactions on behalf of that key
    * ---
    * #### Params
    *
    * | Fields | Data type | Required / Optional | Description |
    * | ---| ---	| --- | --- |
    * | publicKey | String	| Required | Public key corresponding to an encrypted keyfile in your wallet directory |
    * | password  | String	| Required | String used to encrypt the private keyfile that is stored locally |
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return - Json(publicKey -> unlocked) or an error msg.
    */
  private def unlockKeyfile(params: Json, id: String): Future[Json] = {
    val publicKey: String = (params \\ "publicKey").head.asString.get
    val password: String = (params \\ "password").head.asString.get

    (keyManager ? UnlockKeyFile(publicKey, password)).mapTo[Try[Unit]].map {
      case Success(_) => Map(publicKey -> "unlocked".asJson).asJson
      case Failure(ex) => throw new Error(s"An error occurred while trying to unlock the keyfile. $ex")
    }
  }

  /** #### Summary
    * Lock keyfile
    *
    * #### Description
    * Lock a previously unlocked keyfile in your wallet.
    * ---
    * #### Params
    * | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    * |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
    * | publicKey               	| String    	| Required            	| Public key corresponding to an encrypted keyfile in your wallet directory |
    * | password                	| String    	| Required            	| String used to encrypt the private keyfile that is stored locally         |
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return - Json(publicKey -> locked) or an error msg.
    */
  private def lockKeyfile(params: Json, id: String): Future[Json] = {
    val publicKey: String = (params \\ "publicKey").head.asString.get

    (keyManager ? LockKeyFile(publicKey)).mapTo[Try[Unit]].map {
      case Success(_) => Map(publicKey -> "locked".asJson).asJson
      case Failure(ex) => throw new Error(s"An error occurred while trying to lock the keyfile. $ex")
    }
  }

  /** #### Summary
    * Generate Mnemonic Phrase
    *
    * #### Description
    * Generates a mnemonic phrase with the given language using Bip39.
    * ---
    * #### Params
    * | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    * | ------------------------	| ----------	| --------------------	| -----------------------------------------------------------------------	  |
    * | language  |  String 	|  Required	| the corresponding ISO language code for the language to create the mnemonic phrase. |
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return - list of the open key files
    */
  private def generateMnemonic(params: Json, id: String): Future[Json] = {
    val lang = (params \\ "language").head.asString.get
    val phraseTranslator = Bip39.apply(lang)
    val (seed, phrase) = phraseTranslator.uuidSeedPhrase(java.util.UUID.randomUUID.toString)
    Future{Map("seed" -> seed, "mnemonicPhrase" -> phrase).asJson}
  }

}
