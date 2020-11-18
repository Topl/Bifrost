package co.topl.http.api.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.attestation.{Address, Proposition, PublicKeyPropositionCurve25519, ThresholdPropositionCurve25519}
import co.topl.http.api.ApiRouteWithView
import co.topl.modifier.transaction._
import co.topl.nodeView.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.nodeView.state.box.{ArbitBox, PolyBox, TokenBox}
import co.topl.settings.{AppContext, RPCApiSettings}
import io.circe.Json
import io.circe.syntax._
import scorex.util.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

case class WalletApiRoute (override val settings: RPCApiSettings, appContext: AppContext, nodeViewHolderRef: ActorRef )
                          ( implicit val context: ActorRefFactory ) extends ApiRouteWithView {
  type HIS = History
  type MS = State
  type MP = MemPool
  override val route: Route = { basicRoute(handlers) }

  // Establish the expected network prefix for addresses
  implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

  def handlers ( method: String, params: Vector[Json], id: String ): Future[Json] =
    method match {
      case "transferArbitsPrototype" => transferArbitsPrototype(params.head, id)
      case "transferPolysPrototype"  => transferPolysPrototype(params.head, id)
      case "balances"                => balances(params.head, id)
      case "broadcastTx"             => broadcastTx(params.head, id)
    }

  def checkPublicKey ( keys: Seq[Address], view: CV): Unit = {
    if ( !view.state.hasTBR )
      throw new Exception("TokenBoxRegistry not defined for node")

    //YT NOTE - if nodeKeys not defined in settings file then node watches for all keys in a state update
    if ( view.state.nodeKeys.isDefined && !keys.forall(key => view.state.nodeKeys.contains(key)) )
      throw new Exception("Node not set to watch for specified public key")
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
  private def transferPolysPrototype ( implicit params: Json, id: String ): Future[Json] = {
    viewAsync().map { view =>
      // parse arguments from the request
      (for {
        propType <- (params \\ "propositionType").head.as[String]
        recipients <- (params \\ "recipient").head.as[IndexedSeq[(Address, Long)]]
        sender <- (params \\ "sender").head.as[IndexedSeq[Address]]
        changeAddr <- (params \\ "changeAddress").head.as[Address]
        fee <- (params \\ "fee").head.as[Long]
      } yield {
        val data: String = parseOptional("data", "")

        checkPublicKey(sender, view)

        // construct the transaction
        (propType match {
          case PublicKeyPropositionCurve25519.typeString =>
            PolyTransfer
              .createRaw[PublicKeyPropositionCurve25519](view.state, recipients, sender, changeAddr, fee, data)

          case ThresholdPropositionCurve25519.typeString =>
            PolyTransfer
              .createRaw[ThresholdPropositionCurve25519](view.state, recipients, sender, changeAddr, fee, data)
        }) match {
          case Success(tx) => tx
          case Failure(ex) => throw new Error(s"Failed to create raw AssetTransfer with error: $ex")
        }
      }) match {
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
  private def transferArbitsPrototype ( implicit params: Json, id: String ): Future[Json] = {
    viewAsync().map { view =>
      // parse arguments from the request
      (for {
        propType <- (params \\ "propositionType").head.as[String]
        recipients <- (params \\ "recipient").head.as[IndexedSeq[(Address, Long)]]
        sender <- (params \\ "sender").head.as[IndexedSeq[Address]]
        changeAddr <- (params \\ "changeAddress").head.as[Address]
        fee <- (params \\ "fee").head.as[Long]
      } yield {
        val data: String = parseOptional("data", "")

        // check that the state is available
        checkPublicKey(sender, view)

        // construct the transaction
        (propType match {
          case PublicKeyPropositionCurve25519.typeString =>
            ArbitTransfer
              .createRaw[PublicKeyPropositionCurve25519](view.state, recipients, sender, changeAddr, fee, data)

          case ThresholdPropositionCurve25519.typeString =>
            ArbitTransfer
              .createRaw[ThresholdPropositionCurve25519](view.state, recipients, sender, changeAddr, fee, data)
        }) match {
          case Success(tx) => tx
          case Failure(ex) => throw new Error(s"Failed to create raw AssetTransfer with error: $ex")
        }
      }) match {
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

  /** #### Summary
   * Lookup balances
   *
   * #### Type
   * Remote -- Transaction must be used in conjunction with an external key manager service.
   *
   * #### Description
   * Check balances of specified keys.
   *
   * #### Notes
   *    - Requires the Token Box Registry to be active
   *      ---
   *      #### Params
   *      | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
   *      |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
   *      | publicKey               	| String[]   	| Required            	| Public key whose balances are to be retrieved                            	|
   *
   * @param params input parameters as specified above
   * @param id     request identifier
   * @return
   */
  private def balances (params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      // parse arguments from the request
      (for {
        addresses <- (params \\ "addresses").head.as[Seq[Address]]
      } yield {
        // ensure we have the state being asked about
        checkPublicKey(addresses, view)

        val boxes: Map[Address, Map[Byte, Seq[TokenBox]]] =
          addresses
            .map(k => {
              val orderedBoxes = view.state.getTokenBoxes(k) match {
                case Some(boxes) => boxes.groupBy[Byte](b => b.boxTypePrefix)
                case _ => Map[Byte, Seq[TokenBox]]()
              }
              k -> orderedBoxes
            }).toMap

        val balances: Map[Address, Map[Byte, Long]] =
          boxes.map {
            case (addr, assets) => addr -> assets.map {
              case (boxType, boxes) => (boxType, boxes.map(_.value).sum)
            }
          }

        boxes.map {
          case (addr, boxes) =>
            addr -> Map(
              "Balances" -> Map(
                "Polys" -> balances(addr).getOrElse(PolyBox.boxTypePrefix, 0L),
                "Arbits" -> balances(addr).getOrElse(ArbitBox.boxTypePrefix, 0L)
              ).asJson,
              "Boxes" -> boxes.map(b => b._1 -> b._2.asJson).asJson
            )
        }.asJson
      }) match {
        case Right(json) => json
        case Left(ex)    => throw ex
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
  private def broadcastTx ( params: Json, id: String ): Future[Json] = Future {
    (for {
      tx <- (params \\ "tx").head.as[Transaction[_, _ <: Proposition]]
    } yield {
      tx.syntacticValidate
      nodeViewHolderRef ! LocallyGeneratedTransaction(tx)

      tx.asJson
    }) match {
      case Right(json) => json
      case Left(ex) => throw ex
    }
  }
}
