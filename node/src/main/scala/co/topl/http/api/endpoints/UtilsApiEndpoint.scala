package co.topl.http.api.endpoints

import co.topl.attestation.Address
import co.topl.http.api.{ApiEndpoint, Namespace, UtilNamespace}
import co.topl.modifier.box.AssetCode
import co.topl.modifier.box.AssetCode.AssetCodeVersion
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.settings.{AppContext, RPCApiSettings}
import co.topl.utils.NetworkType
import co.topl.utils.NetworkType.NetworkPrefix
import io.circe.Json
import io.circe.syntax._
import co.topl.crypto.hash.{Blake2b256, Hash}
import co.topl.utils.encode.Base58

import java.security.SecureRandom
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class UtilsApiEndpoint(override val settings: RPCApiSettings, appContext: AppContext)
                           (implicit val ec: ExecutionContext) extends ApiEndpoint {

  type HIS = History
  type MS = State
  type MP = MemPool

  // Establish the expected network prefix for addresses
  implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

  // the namespace for the endpoints defined in handlers
  val namespace: Namespace = UtilNamespace

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    case (method, params, id) if method == s"${namespace.name}_seed"              => seedRoute(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_seedOfLength"      => seedOfLength(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_hashBlake2b256"    => hashBlake2b256(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_generateAssetCode" => generateAssetCode(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_checkValidAddress" => checkValidAddress(params.head, id)
  }

  private def generateSeed(length: Int): String = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    Base58.encode(seed)
  }

  /** #### Summary
    * Generates random seed of 32 bytes
    *
    * #### Params
    * | Fields           | Data type | Required / Optional | Description |
    * |------------------|-----------|---------------------|-------------|
    * | -None specified- |           |                     |             |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def seedRoute(params: Json, id: String): Future[Json] = {
    val seedSize = 32 // todo: JAA - read this from a more appropriate place. Bip39 spec or something?
    Future(Map("seed" -> generateSeed(seedSize)).asJson)
  }

  /** #### Summary
    * Generates random seed of specified length
    *
    * #### Params
    * | Fields | Data type | Required / Optional | Description                        |
    * |--------|-----------|---------------------|------------------------------------|
    * | length | Number    | Required            | The number of characters to return |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def seedOfLength(params: Json, id: String): Future[Json] =
    (for {
      length <- params.hcursor.get[Int]("length")
    } yield Future(Map("seed" -> generateSeed(length)).asJson)) match {
      case Right(json) => json
      case Left(ex)    => throw ex
    }

  /** #### Summary
    * Returns Blake2b hash of specified message
    *
    * #### Params
    * | Fields  | Data type | Required / Optional | Description                     |
    * |---------|-----------|---------------------|---------------------------------|
    * | message | String    | Required            | The message that will be hashed |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def hashBlake2b256(params: Json, id: String): Future[Json] =
    (for {
      message <- params.hcursor.get[String]("message")
    } yield Future(
      Map(
        "message" -> message,
        "hash"    -> Base58.encode(Hash[Blake2b256](message).toBytes)
      ).asJson
    )) match {
      case Right(json) => json
      case Left(ex)    => throw ex
    }

  /** #### Summary
    * Returns an encoded assetCode generated from provided parameters
    *
    * #### Params
    * | Fields    | Data type | Required / Optional | Description                                      |
    * |-----------|-----------|---------------------|--------------------------------------------------|
    * | version   | String    | Required            | AssetCode version(version 1 would be string "1") |
    * | issuer    | String    | Required            | The Address of the asset issuer                  |
    * | shortName | String    | Required            | A Latin-1 encoded string of up to 8 characters     |
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return
    */
  private def generateAssetCode(params: Json, id: String): Future[Json] = Future {
    (for {
      version   <- params.hcursor.get[AssetCodeVersion]("version")
      issuer    <- params.hcursor.get[Address]("issuer")
      shortName <- params.hcursor.get[String]("shortName")
    } yield Try(AssetCode(version, issuer, shortName))) match {
      case Right(Success(assetCode)) =>
        Map(
          "assetCode" -> assetCode.asJson
        ).asJson
      case Right(Failure(ex)) => throw new Exception(s"Unable to generate assetCode: $ex")
      case Left(ex)           => throw ex
    }
  }

  /** #### Summary
    * Check if the provided address is valid, returns the address and network type
    *
    * #### Params
    * | Fields  | Data type | Required / Optional | Description                                    |
    * |---------|-----------|---------------------|------------------------------------------------|
    * | network | String    | Required            | A Latin-1 encoded string of up to 8 characters |
    * | address | String    | Required            | The Address of the asset issuer                |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def checkValidAddress(params: Json, id: String): Future[Json] = Future {
    (params.hcursor.get[Option[String]]("network") match {
      // case if no network specified for query
      case Right(None) =>
        val nt = NetworkType.pickNetworkType(networkPrefix).get
        (nt.verboseName, params.hcursor.get[Address]("address"))

      // case if a specific network type is being queried
      case Right(Some(networkName)) =>
        NetworkType.pickNetworkType(networkName) match {
          case None => throw new Exception("Invalid network specified")
          case Some(nt) =>
            implicit val networkPrefix: NetworkPrefix = nt.netPrefix
            (nt.verboseName, params.hcursor.get[Address]("address"))
        }

      case Left(ex) => throw ex

    }) match {
      // successfully API response
      case (networkName: String, Right(address)) =>
        Map(
          "address" -> address.asJson,
          "network" -> networkName.asJson
        ).asJson

      // error passing
      case (_, Left(ex)) => throw ex
    }
  }
}
