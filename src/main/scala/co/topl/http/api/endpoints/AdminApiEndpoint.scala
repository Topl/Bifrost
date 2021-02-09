package co.topl.http.api.endpoints

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import co.topl.attestation.Address
import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.consensus.Forger.ReceivableMessages._
import co.topl.http.api.{AdminNamespace, ApiEndpoint, Namespace}
import co.topl.settings.{AppContext, RPCApiSettings}
import io.circe.{DecodingFailure, Json}
import io.circe.syntax.EncoderOps

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

case class AdminApiEndpoint(override val settings: RPCApiSettings, appContext: AppContext, keyHolderRef: ActorRef)
                           (implicit val context:  ActorRefFactory) extends ApiEndpoint {

  // Establish the expected network prefix for addresses
  implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

  // the namespace for the endpoints defined in handlers
  val namespace: Namespace = AdminNamespace

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    case (method, params, id) if method == s"${namespace.name}_unlockKeyfile"    => unlockKeyfile(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_lockKeyfile"      => lockKeyfile(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_generateKeyfile"  => generateKeyfile(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_importSeedPhrase" => importKeyfile(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_listOpenKeyfiles" => listOpenKeyfiles(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_startForging"     => startForging(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_stopForging"      => stopForging(params.head, id)
  }

  /** #### Summary
    * Unlock keyfile
    *
    * #### Type
    * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
    *
    * #### Description
    * Unlock an encrypted keyfile which exists in your keyfile directory. This will add the secret key to wallet and
    * allow signing of transactions on behalf of that key
    *
    * #### Params
    * | Fields | Data type | Required / Optional | Description |
    * | --- | --- | --- | --- |
    * | address | String | Required | Address corresponding to an encrypted keyfile in your wallet directory |
    * | password | String | Required | String used to encrypt the private keyfile that is stored locally |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def unlockKeyfile(params: Json, id: String): Future[Json] = {
    (for {
      address <- params.hcursor.get[String]("address")
      password <- params.hcursor.get[String]("password")
    } yield (keyHolderRef ? UnlockKey(address, password)).mapTo[Try[Unit]].map {
      case Success(_) => Map(address -> "unlocked".asJson).asJson
      case Failure(ex) => throw new Error(s"An error occurred while trying to unlock the keyfile. $ex")
    }) match {
      case Right(json) => json
      case Left(ex) => throw ex
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
    *
    * #### Params
    * | Fields | Data type | Required / Optional | Description |
    * | --- | --- | --- | --- |
    * | address | String | Required | Address corresponding to an encrypted keyfile in your wallet directory |
    * | password | String | Required | String used to encrypt the private keyfile that is stored locally |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def lockKeyfile(params: Json, id: String): Future[Json] =
    (for {
      addr <- params.hcursor.get[Address]("address")
    } yield (keyHolderRef ? LockKey(addr)).mapTo[Try[Unit]].map {
      case Success(_)  => Map(addr -> "locked".asJson).asJson
      case Failure(ex) => throw new Exception(s"An error occurred while trying to lock the keyfile. $ex")
    }) match {
      case Right(json) => json
      case Left(ex)    => throw ex
    }

  /** #### Summary
    * Generate a new keyfile in local storage
    *
    * #### Type
    * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
    *
    * #### Description
    * Generate and save a new encrypted private keyfile using Curve25519 key pairs.
    *
    * #### Params
    * | Fields | Data type | Required / Optional | Description |
    * | --- | --- | --- | --- |
    * | password | String | Required | String used to encrypt the private keyfile that is stored locally |
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return
    */
  private def generateKeyfile(params: Json, id: String): Future[Json] = {
    (for {
      password <- params.hcursor.get[String]("password")
    } yield (keyHolderRef ? CreateKey(password)).mapTo[Try[Address]].map {
      case Success(addr: Address) => Map("address" -> addr.asJson).asJson
      case Failure(ex)            => throw new Error(s"An error occurred while creating a new keyfile. $ex")
    }) match {
      case Right(json) => json
      case Left(ex)    => throw ex
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
    *
    * #### Params
    * | Fields | Data type | Required / Optional | Description |
    * | --- | --- | --- | --- |
    * | password | String | Required | String used to encrypt the private keyfile that is stored locally |
    * | seedPhrase | String | Required | 12, 15, 18, 21, or 24 word mnemonic |
    * | seddPhraseLang | String | Optional | Defaults to 'en'. Valid options are ["zh-hans", "zh-hant", "en", "fr", "it", "ja", "ko", "es"] |
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return
    */
  private def importKeyfile(implicit params: Json, id: String): Future[Json] = {
    (for {
      password <- params.hcursor.get[String]("password")
      seedPhrase <- params.hcursor.get[String]("seedPhrase")
      seedPhraseLang <- params.hcursor.get[Option[String]]("seedPhrase")
    } yield (keyHolderRef ? ImportKey(password, seedPhrase, seedPhraseLang)).mapTo[Try[Address]].map {
      case Success(addr: Address) => Map("publicKey" -> addr.asJson).asJson
      case Failure(ex)            => throw new Error(s"An error occurred while importing the seed phrase. $ex")
    }) match {
      case Right(json) => json
      case Left(ex)    => throw ex
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
    *
    * #### Params
    * | Fields | Data type | Required / Optional | Description |
    * | --- | --- | --- | --- |
    * | --None specified-- |  |  |  |
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return
    */
  private def listOpenKeyfiles(params: Json, id: String): Future[Json] =
    (keyHolderRef ? ListKeys).mapTo[Set[Address]].map { b =>
      Map("unlocked" -> b.map(_.toString).asJson).asJson
    }

  /** #### Summary
    * Send the start forging signal
    *
    * #### Type
    * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
    *
    * #### Description
    * Attempt to forge blocks using any unlocked keyfiles available on the node
    *
    * #### Params
    * | Fields | Data type | Required / Optional | Description |
    * | --- | --- | --- | --- |
    * | --None specified-- |  |  |  |
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return
    */
  private def startForging(params: Json, id: String): Future[Json] = Future {
    keyHolderRef ! StartForging
    Map("msg" -> "START forging signal sent".asJson).asJson
  }

  /** #### Summary
    * Send the stop forging signal
    *
    * #### Type
    * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
    *
    * #### Description
    * Attempt to stop forging blocks
    *
    * #### Params
    * | Fields | Data type | Required / Optional | Description |
    * | --- | --- | --- | --- |
    * | --None specified-- |  |  |  |
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return
    */
  private def stopForging(params: Json, id: String): Future[Json] = Future {
    keyHolderRef ! StopForging
    Map("msg" -> "STOP forging signal sent".asJson).asJson
  }
}
