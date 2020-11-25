package co.topl.http.api.endpoints

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.consensus.Forger.ReceivableMessages._
import co.topl.http.api.ApiEndpoint
import co.topl.settings.{AppContext, RPCApiSettings}
import io.circe.Json
import io.circe.syntax.EncoderOps

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

case class KeyManagementApiEndpoint (override val settings: RPCApiSettings, appContext: AppContext, keyHolderRef: ActorRef )
                                    ( implicit val context: ActorRefFactory ) extends ApiEndpoint {

  // Establish the expected network prefix for addresses
  implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    case ("admin_unlockKeyfile", params, id)    => unlockKeyfile(params.head, id)
    case ("admin_lockKeyfile", params, id)      => lockKeyfile(params.head, id)
    case ("admin_generateKeyfile", params, id)  => generateKeyfile(params.head, id)
    case ("admin_importSeedPhrase", params, id) => importKeyfile(params.head, id)
    case ("admin_listOpenKeyfiles", params, id) => listOpenKeyfiles(params.head, id)
  }

  /** #### Summary
   * Unlock keyfile
   *
   * #### Type
   * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
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
   * @return
   */
  private def unlockKeyfile ( params: Json, id: String ): Future[Json] = {
    val publicKey: String = (params \\ "publicKey").head.asString.get
    val password: String = (params \\ "password").head.asString.get

    (keyHolderRef ? UnlockKey(publicKey, password)).mapTo[Try[Unit]].map {
      case Success(_)  => Map( publicKey -> "unlocked".asJson).asJson
      case Failure(ex) => throw new Error(s"An error occurred while trying to unlock the keyfile. $ex")
    }
  }

  /** #### Summary
   * Lock keyfile
   *
   * #### Type
   * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
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
   * @return
   */
  private def lockKeyfile ( params: Json, id: String ): Future[Json] = {
    val publicKey: String = (params \\ "publicKey").head.asString.get
    val password: String = (params \\ "password").head.asString.get

    (keyHolderRef ? LockKey(publicKey, password)).mapTo[Try[Unit]].map {
      case Success(_)  => Map(publicKey -> "locked".asJson).asJson
      case Failure(ex) => throw new Error(s"An error occurred while trying to lock the keyfile. $ex")
    }
  }

  /** #### Summary
   * Generate a new keyfile in local storage
   *
   * #### Type
   * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
   *
   * #### Description
   * Generate and save a new encrypted private keyfile using Curve25519 key pairs.
   * ---
   * #### Params
   * | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
   * |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
   * | password                	| String    	| Required            	| String used to encrypt the private keyfile that is stored locally        	|
   *
   * @param params input parameters as specified above
   * @param id     request identifier
   * @return
   */
  private def generateKeyfile ( params: Json, id: String ): Future[Json] = {
    val password: String = (params \\ "password").head.asString.get

    (keyHolderRef ? CreateKey(password)).mapTo[Try[Address]].map {
      case Success(addr: Address) => Map("address" -> addr.asJson).asJson
      case Failure(ex)            => throw new Error(s"An error occurred while creating a new keyfile. $ex")
    }
  }

  /** #### Summary
   * Import key from mnemonic
   *
   * #### Type
   * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
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
   * @return
   */
  private def importKeyfile ( implicit params: Json, id: String ): Future[Json] = {
    val password: String = (params \\ "password").head.asString.get
    val seedPhrase: String = (params \\ "seedPhrase").head.asString.get
    val seedPhraseLang: String = parseOptional("seedPhraseLang", "en")

    (keyHolderRef ? ImportKey(password, seedPhrase, seedPhraseLang)).mapTo[Try[Address]].map {
      case Success(addr: Address) => Map("publicKey" -> addr.asJson).asJson
      case Failure(ex)            => throw new Error(s"An error occurred while importing the seed phrase. $ex")
    }
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
   * @return
   */
  private def listOpenKeyfiles ( params: Json, id: String ): Future[Json] = {
    (keyHolderRef ? ListKeys).mapTo[Set[Address]].map { b =>
      Map("unlocked" -> b.map(_.toString).asJson).asJson
    }
  }
}