package co.topl.http.api.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import co.topl.attestation.Address
import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.attestation.proposition.{PublicKeyPropositionCurve25519, ThresholdPropositionCurve25519}
import co.topl.http.api.ApiRouteWithView
import co.topl.modifier.transaction.AssetTransfer
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.settings.{AppContext, RESTApiSettings}
import io.circe.Json
import io.circe.syntax._
import scorex.util.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}


/** Class route for managing assets using JSON-RPC requests
  *
  * @param nodeViewHolderRef actor reference to inform of new transactions
  * @param settings the settings for HTTP REST API
  * @param context reference to the actor system used to create new actors for handling requests
  */
case class AssetApiRoute(settings: RESTApiSettings, appContext: AppContext, nodeViewHolderRef: ActorRef)
                        (implicit val context: ActorRefFactory) extends ApiRouteWithView {
  type HIS = History
  type MS = State
  type MP = MemPool
  override val route: Route = { basicRoute(handlers) }

  // Establish the expected network prefix for addresses
  implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

  def handlers(method: String, params: Vector[Json], id: String): Future[Json] =
    method match {
      case "transferAssetsPrototype" => transferAssetsPrototype(params.head, id)
    }

  /** #### Summary
    *    Transfer assets from an account to a specified recipient.
    * 
    *  #### Type
    *    Remote -- Transaction must be used in conjunction with an external key manager service.
    * 
    *  #### Description
    *    Default behavior of the wallet is to find the first unlocked address which hold the targetted asset.
    *    The protocols default behavior is to combine multiple UTXOs of the same type into a single UTXO when it can.
    * 
    *  #### Notes
    *    - Change is returned to the first sender in the array of senders 
    * ---
    *  #### Params
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
    *  | issuer                  	| String    	| Required            	| Asset issuer used to identify asset                                    	  |
    *  | assetCode               	| String    	| Required            	| Name of asset                                                          	  |
    *  | recipient               	| String    	| Required            	| Public key of the transfer recipient                                   	  |
    *  | sender                  	| String[]   	| Required            	| Array of public keys from which assets should be sent                   	|
    *  | amount                  	| Number     	| Required            	| Amount of asset to send                                                	  |
    *  | fee                     	| Number     	| Optional            	| **Currently unused**                                                   	  |
    *  | data                    	| String    	| Optional            	| Data string which can be associated with this transaction (may be empty) 	|
    *
    * @param params input parameter as specified above
    * @param id request identifier
    * @return
    */
  private def transferAssetsPrototype(implicit params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      // parse arguments from the request
      val tx = for {
        propType <- (params \\ "propositionType").head.as[String]
        recipients <- (params \\ "recipient").head.as[IndexedSeq[(Address, Long)]]
        sender <- (params \\ "sender").head.as[IndexedSeq[Address]]
        changeAddr <- (params \\ "changeAddress").head.as[Address]
        fee <- (params \\ "fee").head.as[Long]
        issuer <- (params \\ "issuer").head.as[Address]
        assetCode <- (params \\ "assetCode").head.as[String]
        minting <- (params \\ "minting").head.as[Boolean]
      } yield {
        val data: String = parseOptional("data", "")

        // check that the transaction can be constructed
        if ( !view.state.hasTBR )
          throw new Exception("TokenBoxRegistry not defined for node")

        //YT NOTE - if nodeKeys not defined in settings file then node watches for all keys in a state update
        if ( view.state.nodeKeys.isDefined && !sender.forall(view.state.nodeKeys.contains(_)) )
          throw new Exception("Node not set to watch for specified public key")

        // construct the transaction
        (propType match {
          case PublicKeyPropositionCurve25519.typeString =>
            AssetTransfer
              .createRaw[PublicKeyPropositionCurve25519](view.state, recipients, sender, changeAddr, issuer, assetCode, fee, data, minting)

          case ThresholdPropositionCurve25519.typeString =>
            AssetTransfer
              .createRaw[ThresholdPropositionCurve25519](view.state, recipients, sender, changeAddr, issuer, assetCode, fee, data, minting)
        }) match {
          case Success(tx) => tx
          case Failure(ex) => throw new Error(s"Failed to create raw AssetTransfer with error: $ex")
        }
      }

      tx match {
        case Right(tx) =>
          // validate and update nodeView with new TX
          tx.rawValidate match {
            case Success(_) =>
              Map(
                "rawTx" -> tx.asJson,
                "messageToSign" -> Base58.encode(tx.messageToSign).asJson
              ).asJson
            case Failure(e) =>
              throw new Exception(s"Could not validate transaction: $e")
          }


        case Left(ex) => throw ex
      }
    }
  }
}
