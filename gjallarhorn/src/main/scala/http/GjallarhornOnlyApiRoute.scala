package http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import attestation.Address
import attestation.AddressEncoder.NetworkPrefix
import io.circe.Json
import io.circe.syntax._
import keymanager.KeyManager.{ChangeNetwork, GenerateSignatures, SignTx}
import keymanager.networkPrefix
import requests.ApiRoute
import settings.AppSettings

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

case class GjallarhornOnlyApiRoute (settings: AppSettings,
                                    keyManagerRef: ActorRef)
                                   (implicit val context: ActorRefFactory)
  extends ApiRoute {

  val namespace: Namespace = WalletNamespace

  // Establish the expected network prefix for addresses
  implicit val netPrefix: NetworkPrefix = networkPrefix

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    //TODO: enable gjallarhorn to create raw transaction.
    //case (method, params, id) if method == s"${namespace.name}_createRawTransaction" =>
    // createRawTransaction(params.head, id)

    case (method, params, id) if method == s"${namespace.name}_signTx" => signTx(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_networkType" =>
      Future{Map("networkPrefix" -> networkPrefix).asJson}
    case (method, params, id) if method == s"${namespace.name}_changeNetwork" => changeNetwork(params.head, id)
  }

  /** #### Summary
    *    Create raw transaction.
    *
    *  #### Description
    *    Default behavior of the wallet is to find the first unlocked address which hold the targetted asset.
    *    The protocols default behavior is to combine multiple UTXOs of the same type into a single UTXO when it can.
    *
    * ---
    *  #### Params
    *  | Fields    | Data type | Required / Optional | Description                                                            |
    *  |-----------|-----------|---------------------|------------------------------------------------------------------------|
    *  | transferType          | String                            | Required            | either Poly, Asset, or Arbit
    *  | propositionType       | String                            | Required            | either PublicKeyCurve25519 or ThresholdPropositionCurve25519
    *  | assetCode             | String                            | Required            | Name of asset                                                  |
    *  | recipients            | IndexedSeq[(Address, AssetValue)] | Required            | Recipients and asset values to be sent                         |
    *  | sender                | Address[]                         | Required            | Array of public keys from which assets should be sent          |
    *  | changeAddress         | Address                           | Required            | Address to return change to
    *  | consolidationAddress  | Address                           | Optional            | Address to return leftover arbits or assets to         |
    *  | fee                   | Number                            | Required            | **Currently unused**                                       |
    *  | minting               | Boolean                           | Required            | If new asset creation                               |
    *  | data                  | String                            | Optional            | Data string which can be associated with this transaction(may be empty)|
    *
    * //@param params input parameter as specified above
    * //@param id request identifier
    * @return
    */
/*  private def createRawTransaction (params: Json, id: String): Future[Json] = {
    (for {
      transferType <- (params \\ "transferType").head.as[String]
    } yield {
      transferType match {
        case "poly" =>
        case "arbit" =>
        case "asset" =>
      }
    }) match {
        case Right(value) => value
        case Left(error) => throw new Exception(s"error parsing raw tx: $error")
      }
  }*/

  /** #### Summary
    * Sign transaction
    *
    * #### Description
    * Signs a transaction - adds a signature to a raw transaction.
    * ---
    * #### Params
    *
    * | Fields | Data type | Required / Optional | Description |
    * | ---| ---	| --- | --- |
    * | rawTx | Json	| Required | The transaction to be signed. |
    * | signingKeys | IndexedSeq[Address]	| Required | Keys used to create signatures to sign tx.|
    * | messageToSign | String | Required | The message to sign - in the form of an array of bytes.|
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return - transaction with signatures filled in.
    */
  private def signTx(params: Json, id: String): Future[Json] = {
    val tx = (params \\ "rawTx").head
    (for {
      signingKeys <- (params \\ "signingKeys").head.as[IndexedSeq[Address]]
      messageToSign <- (params \\ "messageToSign").head.as[String]
    } yield {
      if (tx.asObject.isDefined) {
        (keyManagerRef ? SignTx(tx, signingKeys, messageToSign)).mapTo[Json]
      } else {
        (keyManagerRef ? GenerateSignatures(signingKeys, messageToSign)).mapTo[Json]
      }
    }) match {
      case Right(value) => value
      case Left(error) => throw new Exception(s"error parsing signing keys: $error")
    }
  }

  /** #### Summary
    * Change network
    *
    * #### Description
    * Changes the current network to the given network.
    * ---
    * #### Params
    *
    * | Fields | Data type | Required / Optional | Description |
    * | ---| ---	| --- | --- |
    * | newNetwork | String	| Required | the new network to switch to |
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return - "newNetworkPrefix" -> networkPrefix or an error message if the network name is not valid.
    */
  private def changeNetwork(params: Json, id: String): Future[Json] = {
    (for {
      newNetwork <- (params \\ "newNetwork").head.as[String]
    } yield {
      (keyManagerRef ? ChangeNetwork(newNetwork)).mapTo[Json]
    }) match {
      case Right(value) => value
      case Left(error) => throw new Exception (s"error parsing new network: $error")
    }
  }

}
