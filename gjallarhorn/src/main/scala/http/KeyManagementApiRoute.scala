package http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import attestation.Address
import attestation.AddressEncoder.NetworkPrefix
import io.circe.Json
import io.circe.syntax._
import keymanager.KeyManager.{GenerateKeyFile, ImportKeyfile, LockKeyFile, UnlockKeyFile, _}
import keymanager.{networkPrefix, Bip39}
import requests.ApiRoute
import settings.RPCApiSettings

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

/**
 * Class route for managing key management requests
 * @param settings - settings for ApiRoute
 * @param keyManager - actor reference for the KeyManager
 * @param context - ActorRef context
 */
case class KeyManagementApiRoute(settings: RPCApiSettings, keyManager: ActorRef)(implicit val context: ActorRefFactory)
    extends ApiRoute {

  // Establish the expected network prefix for addresses
  implicit val netPrefix: NetworkPrefix = networkPrefix

  //The namespace for the endpoints defined in handlers
  val namespace: Namespace = WalletNamespace

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    case (method, params, id) if method == s"${namespace.name}_generateKeyfile"  => generateKeyfile(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_generateMnemonic" => generateMnemonic(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_importKeyfile"    => importKeyfile(params.head, id)

    case (method, params, id) if method == s"${namespace.name}_unlockKeyfile" => unlockKeyfile(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_lockKeyfile"   => lockKeyfile(params.head, id)

    case (method, params, id) if method == s"${namespace.name}_listOpenKeyfiles" => listOpenKeyfiles(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_listAllKeyfiles"  => listAllKeyfiles(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_getKeyfileDir"    => getKeyfileDir(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_changeKeyfileDir" => changeKeyfileDir(params.head, id)
  }

  /**
   * #### Summary
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
  private def listOpenKeyfiles(params: Json, id: String): Future[Json] =
    (keyManager ? GetOpenKeyfiles).mapTo[Set[Address]].map(_.asJson)

  /**
   * #### Summary
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
   * @return - mapping of address to "locked" or "unlocked"
   */
  private def listAllKeyfiles(params: Json, id: String): Future[Json] =
    (keyManager ? GetAllKeyfiles).mapTo[Map[Address, String]].map(_.asJson)

  /**
   * #### Summary
   * Generates a key file
   *
   * #### Description
   * Grabs all of the keyfiles in a given directory
   * ---
   * #### Params
   * | Fields       | Data type 	| Required / Optional 	| Description                              	  |
   * | -------------| ----------	| --------------------	| ----------------------------------------	  |
   * | password     |  String   	|   Required           	|  password for the generated key file        |
   * | seed         |  String   	|   Optional           	|  optional seed used to generate key file    |
   *
   * @param params - input parameters as specified above
   * @param id - request identifier
   * @return - the address for generated keyfile.
   */
  private def generateKeyfile(params: Json, id: String): Future[Json] = {
    val p = params.hcursor
    (for {
      password <- p.get[String]("password")
      seed     <- p.get[Option[String]]("seed")
    } yield (keyManager ? GenerateKeyFile(password, seed)).mapTo[Try[Address]].map {
      case Success(pk: Address) => Map("address" -> pk.asJson).asJson
      case Failure(ex)          => throw new Exception(s"An error occurred while creating a new keyfile. $ex")
    }) match {
      case Right(value) => value
      case Left(ex)     => throw new Exception(s"parsing error when generating keyfile: $ex")
    }
  }

  /**
   * #### Summary
   * Imports key file from mnemonic phrase
   *
   * #### Description
   * Allows a user to import a 12, 15, 18, 21, or 24 word mnemonic (seed phrase) and generate an encrypted Keyfile
   * ---
   * #### Params
   * | Fields         | Data type 	| Required / Optional | Description                                              |
   * |----------------|-----------	|-------------------- |----------------------------------------------------------|
   * | password       | String    	| Required   | String used to encrypt the private keyfile that is stored locally |
   * | seedPhrase     | String    	| Required   | 12, 15, 18, 21, or 24 word mnemonic							     |
   * | seedPhraseLang | String    	| Optional   | Defaults to 'en'. Valid options are ["zh-hans", "zh-hant", "en", "fr", "it", "ja", "ko", "es"] |
   *
   * @param params input parameters as specified above
   * @param id     request identifier
   * @return - returns the address for the imported key.
   */
  private def importKeyfile(implicit params: Json, id: String): Future[Json] =
    (for {
      password   <- (params \\ "password").head.as[String]
      seedPhrase <- (params \\ "seedPhrase").head.as[String]
    } yield (keyManager ? ImportKeyfile(password, seedPhrase, parseOptional("seedPhraseLang", "en")))
      .mapTo[Try[Address]]
      .map {
        case Success(pk: Address) => Map("address" -> pk.asJson).asJson
        case Failure(ex)          => throw new Exception(s"Error importing key file: $ex")
      }) match {
      case Right(value) => value
      case Left(ex)     => throw new Exception(s"parsing error when importing keyfile: $ex")
    }

  /**
   * #### Summary
   * Unlock keyfile
   *
   * #### Description
   * Unlock an encrypted keyfile which exists in your keyfile directory.
   * This will add the secret key to the wallet and allow signing of transactions on behalf of that key
   * ---
   * #### Params
   *
   * | Fields | Data type | Required / Optional | Description |
   * | ---| ---	| --- | --- |
   * | address   | String	| Required | Address corresponding to an encrypted keyfile in your wallet directory |
   * | password  | String	| Required | String used to encrypt the private keyfile that is stored locally |
   *
   * @param params input parameters as specified above
   * @param id     request identifier
   * @return - Json(address -> unlocked) or an error msg.
   */
  private def unlockKeyfile(params: Json, id: String): Future[Json] =
    (for {
      address  <- (params \\ "address").head.as[String]
      password <- (params \\ "password").head.as[String]
    } yield (keyManager ? UnlockKeyFile(address, password)).mapTo[Try[Unit]].map {
      case Success(_)  => Map(address -> "unlocked".asJson).asJson
      case Failure(ex) => throw new Exception(s"An error occurred while trying to unlock the keyfile. $ex")
    }) match {
      case Right(value) => value
      case Left(ex)     => throw new Exception(s"parsing error when unlocking keyfile: $ex")
    }

  /**
   * #### Summary
   * Lock keyfile
   *
   * #### Description
   * Lock a previously unlocked keyfile in your wallet.
   * ---
   * #### Params
   * | Fields    | Data type 	| Required / Optional | Description                                                   |
   * |-----------|-----------	|------------|------------------------------------------------------------------------|
   * | address   | String    	| Required   | Address corresponding to an encrypted keyfile in your wallet directory |
   *
   * @param params input parameters as specified above
   * @param id     request identifier
   * @return - Json(address -> locked) or an error msg.
   */
  private def lockKeyfile(params: Json, id: String): Future[Json] = {
    val address: String = (params \\ "address").head.asString.get

    (keyManager ? LockKeyFile(address)).mapTo[Try[Unit]].map {
      case Success(_)  => Map(address -> "locked".asJson).asJson
      case Failure(ex) => throw new Exception(s"An error occurred while trying to lock the keyfile. $ex")
    }
  }

  /**
   * #### Summary
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
    Future(Map("seed" -> seed, "mnemonicPhrase" -> phrase).asJson)
  }

  /**
   * #### Summary
   * Get current keyfile directory file path
   *
   * ---
   * #### Params
   *
   * | Fields | Data type | Required / Optional | Description |
   * | ---| ---	| --- | --- |
   * | --None specified--    |
   *
   * @param params input parameters as specified above
   * @param id     request identifier
   * @return - keyfile directory path (the absolute path)
   */
  private def getKeyfileDir(params: Json, id: String): Future[Json] =
    (keyManager ? GetKeyfileDir).mapTo[Json]

  /**
   * #### Summary
   * Change keyfile directory
   *
   * #### Description
   * Changes the current keyfile directory to the given keyfile directory.
   * ---
   * #### Params
   *
   * | Fields | Data type | Required / Optional | Description |
   * | ---| ---	| --- | --- |
   * | directory | String	| Required | the new directory to switch to |
   *
   * @param params input parameters as specified above
   * @param id     request identifier
   * @return - json mapping: "newDirectory" -> settings.keyFileDir
   */
  private def changeKeyfileDir(params: Json, id: String): Future[Json] =
    (for {
      directory <- (params \\ "directory").head.as[String]
    } yield (keyManager ? ChangeKeyfileDir(directory)).mapTo[Json]) match {
      case Right(value) => value
      case Left(error)  => throw new Exception(s"error parsing directory: $error")
    }

}
