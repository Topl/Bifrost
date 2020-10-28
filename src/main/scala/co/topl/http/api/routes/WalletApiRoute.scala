package co.topl.http.api.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import co.topl.http.api.ApiRouteWithView
import co.topl.modifier.transaction._
import co.topl.nodeView.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.nodeView.state.box.TokenBox
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.settings.RESTApiSettings
import io.circe.Json
import io.circe.syntax._
import scorex.util.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

case class WalletApiRoute ( override val settings: RESTApiSettings, nodeViewHolderRef: ActorRef )
                          ( implicit val context: ActorRefFactory ) extends ApiRouteWithView {
  type HIS = History
  type MS = State
  type MP = MemPool
  override val route: Route = pathPrefix("wallet") { basicRoute(handlers) }

  def handlers ( method: String, params: Vector[Json], id: String ): Future[Json] =
    method match {
//      case "transferPolys"           => transferPolys(params.head, id)
//      case "transferArbits"          => transferArbits(params.head, id)
      case "transferArbitsPrototype" => transferArbitsPrototype(params.head, id)
      case "transferPolysPrototype"  => transferPolysPrototype(params.head, id)
      case "balances"                => balances(params.head, id)
//      case "signTx"                  => signTx(params.head, id)
      case "broadcastTx"             => broadcastTx(params.head, id)
    }

  def checkPublicKey (keys: Seq[PublicKey25519Proposition], view: CV): Unit = {
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
   * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
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
   *      | amount                  	| Number     	| Required            	| Amount of asset to send                                                	  |
   *      | fee                     	| Number     	| Optional            	| Default to 0                                                          	  |
   *      | data                    	| String    	| Optional            	| Data string which can be associated with this transaction (may be empty) 	|
   *
//   * @param params input parameters as specified above
//   * @param id     request identifier
//   * @return
//   */
//  private def transferPolys ( params: Json, id: String ): Future[Json] = {
//    viewAsync().map { view =>
//      val wallet = view.vault
//      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
//      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(
//        Base58.decode((params \\ "recipient").head.asString.get).get
//        )
//      val sender: IndexedSeq[PublicKey25519Proposition] =
//        (params \\ "sender").head.asArray.get
//          .map(key => PublicKey25519Proposition(Base58.decode(key.asString.get).get))
//      val fee: Long =
//        (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
//
//      // Optional API parameters
//      val data: String = (params \\ "data").headOption match {
//        case Some(dataStr) => dataStr.asString.getOrElse("")
//        case None          => ""
//      }
//
//      checkPublicKey(sender, view)
//
//      // Call to BifrostTX to create TX
//      val tx = PolyTransfer
//        .create(
//          view.state.tbrOpt.get,
//          view.state,
//          wallet,
//          IndexedSeq((recipient, amount)),
//          sender,
//          fee,
//          data
//          )
//        .get
//
//      // Update nodeView with new TX
//      PolyTransfer.semanticValidate(tx, view.state) match {
//        case Success(_) =>
//          nodeViewHolderRef ! LocallyGeneratedTransaction[PolyTransfer](tx)
//          tx.json
//        case Failure(e) =>
//          throw new Exception(s"Could not validate transaction: $e")
//      }
//    }
//  }

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
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition((params \\ "recipient").head.asString.get)
      val sender: IndexedSeq[PublicKey25519Proposition] =
        (params \\ "sender").head.asArray.get.map(key => PublicKey25519Proposition(key.asString.get))
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).get
      val data: String = parseOptional("data", "")

      checkPublicKey(sender, view)

      val tx = PolyTransfer
        .createPrototype(
          view.state,
          IndexedSeq((recipient, amount)),
          sender,
          fee,
          data
          )
        .get

      // Update nodeView with new TX
      PolyTransfer.validatePrototype(tx) match {
        case Success(_) =>
          tx.json
        case Failure(e) =>
          throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }

  /** #### Summary
   * Transfer Arbits from an account to a specified recipient.
   *
   * #### Type
   * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
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
//   * @param params input parameters as specified above
//   * @param id     request identifier
//   * @return
//   */
//  private def transferArbits ( params: Json, id: String ): Future[Json] = {
//    viewAsync().map { view =>
//      val wallet = view.vault
//      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
//      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(
//        Base58.decode((params \\ "recipient").head.asString.get).get
//        )
//      val sender: IndexedSeq[PublicKey25519Proposition] =
//        (params \\ "sender").head.asArray.get
//          .map(key =>
//                 PublicKey25519Proposition(Base58.decode(key.asString.get).get)
//               )
//      val fee: Long =
//        (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
//      // Optional API parameters
//      val data: String = (params \\ "data").headOption match {
//        case Some(dataStr) => dataStr.asString.getOrElse("")
//        case None          => ""
//      }
//
//      checkPublicKey(sender, view)
//
//      val tx = ArbitTransfer
//        .create(
//          view.state.tbrOpt.get,
//          view.state,
//          wallet,
//          IndexedSeq((recipient, amount)),
//          sender,
//          fee,
//          data
//          )
//        .get
//      // Update nodeView with new TX
//      ArbitTransfer.semanticValidate(tx, view.state) match {
//        case Success(_) =>
//          nodeViewHolderRef ! LocallyGeneratedTransaction[ArbitTransfer](tx)
//          tx.json
//        case Failure(e) =>
//          throw new Exception(s"Could not validate transaction: $e")
//      }
//    }
//  }

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
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition((params \\ "recipient").head.asString.get)
      val sender: IndexedSeq[PublicKey25519Proposition] =
        (params \\ "sender").head.asArray.get.map(key => PublicKey25519Proposition(key.asString.get))
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).get
      val data: String = parseOptional("data", "")

      checkPublicKey(sender, view)

      val tx = ArbitTransfer
        .createPrototype(view.state, IndexedSeq((recipient, amount)), sender, fee, data)
        .get

      // Update nodeView with new TX
      ArbitTransfer.validatePrototype(tx) match {
        case Success(_) =>
          tx.json
        case Failure(e) =>
          throw new Exception(s"Could not validate transaction: $e")
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
  private def balances ( params: Json, id: String ): Future[Json] = {
    viewAsync().map { view =>
      // parse the required arguments from the request
      val publicKeys = (params \\ "publicKeys").head.asArray.get.map(k => PublicKey25519Proposition(k.asString.get))

      checkPublicKey(publicKeys, view)

      val boxes: Map[PublicKey25519Proposition, Map[String, Seq[TokenBox]]] =
        publicKeys
          .map(k => {
            val orderedBoxes = view.state.getTokenBoxes(k) match {
              case Some(boxes) => boxes.groupBy[String](b => b.typeOfBox)
              case _           => Map[String, Seq[TokenBox]]()
            }
            k -> orderedBoxes
          }).toMap


      val balances: Map[PublicKey25519Proposition, Map[String, Long]] =
        boxes.map {
          case (prop, assets) => prop -> assets.map {
            case (boxType, boxes) => (boxType, boxes.map(_.value).sum)
          }
        }

      boxes.map {
        case (prop, boxes) =>
          prop.address -> Map(
            "Balances" -> Map(
              "Polys" -> balances(prop).getOrElse("Poly", 0L),
              "Arbits" -> balances(prop).getOrElse("Arbit", 0L)
              ).asJson,
            "Boxes" -> boxes.map(b => b._1 -> b._2.map(_.json).asJson).asJson
            )
      }.asJson

    }
  }

  /** #### Summary
   * Sign a prototype transaction
   *
   * #### Type
   * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
   *
   * #### Description
   * Generate and return a signature attached to the specified transaction
   *
   * #### Notes
   *    - Currently only enabled for `AssetCreation` and `AssetTransfer` transactions
   *      ---
   *      #### Params
   *      | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
   *      |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
   *      | signingKeys             	| String[]   	| Required            	| List of keys that signatures will be created for                       	  |
   *      | protoTx             	    | object     	| Required            	| A prototype transaction JSON object                                     	|
   *
//   * @param params input parameters as specified above
//   * @param id     request identifier
//   * @return
//   */
//  private def signTx ( params: Json, id: String ): Future[Json] = {
//    viewAsync().map { view =>
//      val wallet = view.vault
//      val props = (params \\ "signingKeys").head.asArray.get.map(k => {
//        PublicKey25519Proposition(Base58.decode(k.asString.get).get)
//      })
//
//      val tx = (params \\ "protoTx").head
//      val txType = (tx \\ "txType").head.asString.get
//      val txInstance = txType match {
//        case "AssetCreation" => tx.as[AssetCreation].right.get
//        case "AssetTransfer" => tx.as[AssetTransfer].right.get
//        case _               =>
//          throw new Exception(s"Could not find valid transaction type $txType")
//      }
//      val signatures: Json = Map(
//        "signatures" -> Transaction
//          .signTx(wallet, props, txInstance.messageToSign)
//          .map(sig => {
//            Base58.encode(sig._1.pubKeyBytes) -> Base58
//              .encode(sig._2.signature)
//              .asJson
//          })
//        ).asJson
//
//      tx.deepMerge(signatures)
//    }
//  }

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
   *      | tx                 	    | object     	| Required            	| A full formatted transaction JSON object (prototype transaction + signatures) |
   *
   * @param params input parameters as specified above
   * @param id     request identifier
   * @return
   */
  private def broadcastTx ( params: Json, id: String ): Future[Json] = {
    viewAsync().map { view =>
      val tx = (params \\ "tx").head
      val txType = (tx \\ "txType").head.asString.get
      val txInstance: Transaction = txType match {
        case "AssetCreation" => tx.as[AssetCreation].right.get
        case "AssetTransfer" => tx.as[AssetTransfer].right.get
        case _               =>
          throw new Exception(s"Could not find valid transaction type $txType")
      }

      State.syntacticValidity(txInstance)
      nodeViewHolderRef ! LocallyGeneratedTransaction[Transaction](txInstance)
      txInstance.json
    }
  }
}
