package co.topl.http.api.endpoints

import akka.actor.ActorRef
import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.attestation.{Address, Proposition, PublicKeyPropositionCurve25519, ThresholdPropositionCurve25519}
import co.topl.http.api.{ApiEndpointWithView, Namespace, ToplNamespace}
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}
import co.topl.nodeView.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.nodeView.state.box.{AssetValue, SimpleValue}
import co.topl.settings.{AppContext, RPCApiSettings}
import io.circe.Json
import io.circe.syntax._
import scorex.util.encode.Base58

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** Class route for managing assets using JSON-RPC requests
  *
  * @param nodeViewHolderRef actor reference to inform of new transactions
  * @param settings the settings for HTTP REST API
  */
case class TransactionApiEndpoint(
  settings:          RPCApiSettings,
  appContext:        AppContext,
  nodeViewHolderRef: ActorRef
)(implicit val ec:   ExecutionContext) extends ApiEndpointWithView {

  type HIS = History
  type MS = State
  type MP = MemPool

  // Establish the expected network prefix for addresses
  implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

  // the namespace for the endpoints defined in handlers
  val namespace: Namespace = ToplNamespace

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    case (method, params, id) if method == s"${namespace.name}_rawAssetTransfer" => rawAssetTransfer(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_rawArbitTransfer" => rawArbitTransfer(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_rawPolyTransfer"  => rawPolyTransfer(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_broadcastTx"      => broadcastTx(params.head, id)
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
    *  | Fields    | Data type | Required / Optional | Description                                                            |
    *  |-----------|-----------|---------------------|------------------------------------------------------------------------|
    *  | issuer    | String    | Required            | Asset issuer used to identify asset                                    |
    *  | assetCode | String    | Required            | Name of asset                                                          |
    *  | recipient | String    | Required            | Public key of the transfer recipient                                   |
    *  | sender    | String[]  | Required            | Array of public keys from which assets should be sent                  |
    *  | amount    | Number    | Required            | Amount of asset to send                                                |
    *  | fee       | Number    | Optional            | **Currently unused**                                                   |
    *  | data      | String    | Optional            | Data string which can be associated with this transaction(may be empty)|
    *
    * @param params input parameter as specified above
    * @param id request identifier
    * @return
    */
  private def rawAssetTransfer(implicit params: Json, id: String): Future[Json] = {
    viewAsync { view =>
      val p = params.hcursor
      // parse arguments from the request
      (for {
        propType          <- p.get[String]("propositionType")
        recipients        <- p.get[IndexedSeq[(Address, AssetValue)]]("recipients")
        sender            <- p.get[IndexedSeq[Address]]("sender")
        changeAddr        <- p.get[Address]("changeAddress")
        consolidationAddr <- p.get[Option[Address]]("consolidationAddress")
        fee               <- p.get[Long]("fee")
        minting           <- p.get[Boolean]("minting")
        data              <- p.get[Option[String]]("data")
      } yield {

        // check that the state is available
        checkAddress(sender, view)

        // construct the transaction
        propType match {
          case PublicKeyPropositionCurve25519.`typeString` =>
            AssetTransfer
              .createRaw[PublicKeyPropositionCurve25519](
                view.state,
                recipients,
                sender,
                changeAddr,
                consolidationAddr,
                fee,
                data,
                minting
              )

          case ThresholdPropositionCurve25519.`typeString` =>
            AssetTransfer
              .createRaw[ThresholdPropositionCurve25519](
                view.state,
                recipients,
                sender,
                changeAddr,
                consolidationAddr,
                fee,
                data,
                minting
              )
        }
      }) match {
        case Right(Success(tx)) =>
          // validate and update nodeView with new TX
          tx.rawValidate match {
            case Success(_) =>
              Map(
                "rawTx"         -> tx.asJson,
                "messageToSign" -> Base58.encode(tx.messageToSign).asJson
              ).asJson

            case Failure(e) =>
              throw new Exception(s"Could not validate transaction: $e")
          }

        case Right(Failure(ex)) => throw new Exception(s"Failed to create raw AssetTransfer with error: $ex")
        case Left(ex)           => throw ex
      }
    }
  }

  /** #### Summary
    * Transfer Polys from an account to a specified recipient.
    *
    * #### Type
    * Remote -- Transaction must be used in conjunction with an external key manager service.
    *
    * #### Description
    * Default behavior of the wallet is to find the first unlocked address which hold Polys.
    * The protocols default behavior is to combine multiple UTXOs of the same type into a single UTXO when it can.
    *
    * #### Notes
    *    - Change is returned to the first sender in the array of senders
    *      ---
    *      #### Params
    *      | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    *      |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
    *      | recipient               	| String    	| Required            	| Public key of the transfer recipient                                   	  |
    *      | sender                  	| String[]   	| Required            	| Array of public keys from which assets should be sent                   	|
    *      | amount                  	| Number    	| Required            	| Amount of asset to send                                                	  |
    *      | fee                     	| Number     	| Optional            	| Default to 0                                                          	  |
    *      | data                    	| String    	| Optional            	| Data string which can be associated with this transaction (may be empty) 	|
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return
    */
  private def rawPolyTransfer(implicit params: Json, id: String): Future[Json] = {
    viewAsync { view =>
      val p = params.hcursor

      // parse arguments from the request
      (for {
        propType   <- p.get[String]("propositionType")
        recipients <- p.get[IndexedSeq[(Address, Long)]]("recipients")
        sender     <- p.get[IndexedSeq[Address]]("sender")
        changeAddr <- p.get[Address]("changeAddress")
        fee        <- p.get[Long]("fee")
        data       <- p.get[Option[String]]("data")
      } yield {

        // check that the state is available
        checkAddress(sender, view)

        // convert to simple value type
        val to = recipients.map(r => r._1 -> SimpleValue(r._2))

        // construct the transaction
        propType match {
          case PublicKeyPropositionCurve25519.`typeString` =>
            PolyTransfer
              .createRaw[PublicKeyPropositionCurve25519](view.state, to, sender, changeAddr, None, fee, data)

          case ThresholdPropositionCurve25519.`typeString` =>
            PolyTransfer
              .createRaw[ThresholdPropositionCurve25519](view.state, to, sender, changeAddr, None, fee, data)
        }
      }) match {
        case Right(Success(tx)) =>
          // validate and update nodeView with new TX
          tx.rawValidate match {
            case Success(_) =>
              Map(
                "rawTx"         -> tx.asJson,
                "messageToSign" -> Base58.encode(tx.messageToSign).asJson
              ).asJson

            case Failure(e) =>
              throw new Exception(s"Could not validate transaction: $e")
          }

        case Right(Failure(ex)) => throw new Exception(s"Failed to create raw PolyTransfer with error: $ex")
        case Left(ex)           => throw ex
      }
    }
  }

  /** #### Summary
    * Transfer Polys from an account to a specified recipient.
    *
    * #### Type
    * Remote -- Transaction must be used in conjunction with an external key manager service.
    *
    * #### Description
    * Default behavior of the wallet is to find the first unlocked address which hold Arbits.
    * The protocols default behavior is to combine multiple UTXOs of the same type into a single UTXO when it can.
    *
    * #### Notes
    *    - Change is returned to the first sender in the array of senders
    *      ---
    *      #### Params
    *      | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    *      |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
    *      | recipient               	| String    	| Required            	| Public key of the transfer recipient                                   	  |
    *      | sender                  	| String[]   	| Required            	| Array of public keys from which assets should be sent                   	|
    *      | amount                  	| Number     	| Required            	| Amount of asset to send                                                	  |
    *      | fee                     	| Number     	| Optional            	| Default to 0                                                          	  |
    *      | data                    	| String    	| Optional            	| Data string which can be associated with this transaction (may be empty) 	|
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return
    */
  private def rawArbitTransfer(implicit params: Json, id: String): Future[Json] = {
    viewAsync { view =>
      val p = params.hcursor

      // parse arguments from the request
      (for {
        propType          <- p.get[String]("propositionType")
        recipients        <- p.get[IndexedSeq[(Address, Long)]]("recipients")
        sender            <- p.get[IndexedSeq[Address]]("sender")
        changeAddr        <- p.get[Address]("changeAddress")
        consolidationAddr <- p.get[Option[Address]]("consolidationAddress")
        fee               <- p.get[Long]("fee")
        data              <- p.get[Option[String]]("data")
      } yield {

        // check that the state is available
        checkAddress(sender, view)

        // convert to simple value type
        val to = recipients.map(r => r._1 -> SimpleValue(r._2))

        // construct the transaction
        propType match {
          case PublicKeyPropositionCurve25519.`typeString` =>
            ArbitTransfer
              .createRaw[PublicKeyPropositionCurve25519](
                view.state,
                to,
                sender,
                changeAddr,
                consolidationAddr,
                fee,
                data
              )

          case ThresholdPropositionCurve25519.`typeString` =>
            ArbitTransfer
              .createRaw[ThresholdPropositionCurve25519](
                view.state,
                to,
                sender,
                changeAddr,
                consolidationAddr,
                fee,
                data
              )
        }
      }) match {
        case Right(Success(tx)) =>
          // validate and update nodeView with new TX
          tx.rawValidate match {
            case Success(_) =>
              Map(
                "rawTx"         -> tx.asJson,
                "messageToSign" -> Base58.encode(tx.messageToSign).asJson
              ).asJson

            case Failure(e) =>
              throw new Exception(s"Could not validate transaction: $e")
          }

        case Right(Failure(ex)) => throw new Exception(s"Failed to create raw ArbitTransfer with error: $ex")
        case Left(ex)           => throw ex
      }
    }
  }

  /** #### Summary
    * Broadcast transaction
    *
    * #### Type
    * Remote -- Route must be used in conjunction with an external key manager service.
    *
    * #### Description
    * Place specified signed transaction into the mempool and broadcast to other nodes
    *
    * #### Notes
    *    - Currently only enabled for `AssetCreation` and `AssetTransfer` transactions
    *      ---
    *      #### Params
    *      | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	      |
    *      |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	      |
    *      | tx                  	    | object     	| Required            	| A full formatted transaction JSON object (prototype transaction + signatures) |
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return
    */
  private def broadcastTx(params: Json, id: String): Future[Json] = Future {
    (for {
      tx <- (params \\ "tx").head.as[Transaction[_, _ <: Proposition]]
    } yield {
      tx.syntacticValidate.map { _ =>
        nodeViewHolderRef ! LocallyGeneratedTransaction(tx)
        tx.asJson
      }
    }) match {
      case Right(Success(json)) => json
      case Right(Failure(ex))   => throw new Exception (ex)
      case Left(ex)             => throw ex
    }
  }
}
