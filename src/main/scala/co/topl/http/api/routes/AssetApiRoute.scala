package co.topl.http.api.routes

import akka.actor.{ ActorRef, ActorRefFactory }
import akka.http.scaladsl.server.Route
import co.topl.http.api.ApiRouteWithView
import co.topl.modifier.transaction.{ AssetCreation, AssetTransfer }
import co.topl.nodeView.GenericNodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import co.topl.nodeView.state.box.{ AssetBox, BoxId }
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.settings.RESTApiSettings
import co.topl.wallet.Wallet
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{ Failure, Success }


/** Class route for managing assets using JSON-RPC requests
  *
  * @param nodeViewHolderRef actor reference to inform of new transactions
  * @param settings the settings for HTTP REST API
  * @param context reference to the actor system used to create new actors for handling requests
  */
case class AssetApiRoute( override val settings: RESTApiSettings, nodeViewHolderRef: ActorRef)
                        (implicit val context: ActorRefFactory) extends ApiRouteWithView {
  type HIS = History
  type MS = State
  type VL = Wallet
  type MP = MemPool
  override val route: Route = pathPrefix("asset") { basicRoute(handlers) }

  def handlers(method: String, params: Vector[Json], id: String): Future[Json] =
    method match {
      case "transferAssets" => transferAssets(params.head, id)
      case "transferAssetsPrototype" => transferAssetsPrototype(params.head, id)
      case "transferTargetAssets" => transferTargetAssets(params.head, id)
      case "transferTargetAssetsPrototype" => transferTargetAssetsPrototype(params.head, id)
      case "createAssets" => createAssets(params.head, id)
      case "createAssetsPrototype" => createAssetsPrototype(params.head, id)
    }

  /**  #### Summary
    *    Transfer assets from an account to a specified recipient.
    * 
    *  #### Type
    *    Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
    * 
    *  #### Description
    *    Default behavior of the wallet is to find the first unlocked address which hold the targeted asset.
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
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def transferAssets(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>

      // parse required arguments from the request
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(
        Base58.decode((params \\ "recipient").head.asString.get).get
      )
      val sender: IndexedSeq[PublicKey25519Proposition] =
        (params \\ "sender").head.asArray.get.map(key =>
          PublicKey25519Proposition(Base58.decode(key.asString.get).get)
        )
      val fee: Long =
        (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      val issuer = PublicKey25519Proposition(
        Base58.decode((params \\ "issuer").head.asString.get).get
      )
      val assetCode: String = (params \\ "assetCode").head.asString.getOrElse("")

      // parse optional arguments from the request
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None          => ""
      }

      // check that the transaction can be constructed
      if (view.state.tbrOpt.isEmpty)
        throw new Exception("TokenBoxRegistry not defined for node")
      if (view.state.nodeKeys.isDefined)
        sender.foreach(key =>
          if (!view.state.nodeKeys.get.contains(key))
            throw new Exception("Node not set to watch for specified public key")
        )

      val tx = AssetTransfer
        .create(view.state.tbrOpt.get, view.state, view.vault, IndexedSeq((recipient, amount)), sender, fee, issuer, assetCode, data)
        .get

      AssetTransfer.semanticValidate(tx, view.state) match {
        case Success(_) =>
          nodeViewHolderRef ! LocallyGeneratedTransaction[AssetTransfer](tx)
          tx.json
        case Failure(e) =>
          throw new Exception(s"Could not validate transaction: $e")
      }
    }
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
  private def transferAssetsPrototype(
      params: Json,
      id: String
  ): Future[Json] = {
    viewAsync().map { view =>
      // parse required arguments from the request
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(
        Base58.decode((params \\ "recipient").head.asString.get).get
      )
      val sender: IndexedSeq[PublicKey25519Proposition] =
        (params \\ "sender").head.asArray.get.map(key =>
          PublicKey25519Proposition(Base58.decode(key.asString.get).get)
        )
      val fee: Long =
        (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      val issuer = PublicKey25519Proposition(
        Base58.decode((params \\ "issuer").head.asString.get).get
      )
      val assetCode: String =
        (params \\ "assetCode").head.asString.getOrElse("")

      // parse optional arguments from the request
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None          => ""
      }

      // check that the transaction can be constructed
      if (view.state.tbrOpt.isEmpty)
        throw new Exception("TokenBoxRegistry not defined for node")
      if (view.state.nodeKeys.isDefined)
        sender.foreach(key =>
                         if (!view.state.nodeKeys.get.contains(key))
                           throw new Exception("Node not set to watch for specified public key")
                       )

      // construct the transaction
      val tx = AssetTransfer
        .createRaw(view.state.tbrOpt.get, view.state, IndexedSeq((recipient, amount)), sender, issuer, assetCode, fee, data)
        .get

      // validate and update nodeView with new TX
      AssetTransfer.validatePrototype(tx) match {
        case Success(_) =>
          Map(
            "formattedTx" -> tx.json,
            "messageToSign" -> Base58.encode(tx.messageToSign).asJson
          ).asJson
        case Failure(e) =>
          throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }

  /**  #### Summary
    *   Target a specific asset box and transfer to another account
    * 
    *  #### Type
    *    Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
    * 
    *  #### Description
    *    The protocols default behavior is to combine multiple UTXOs of the same type into a single UTXO when it can.
    * 
    *  #### Notes
    *    - Change is returned to the first sender in the array of senders 
    * ---
    *  #### Params
    *
    *  | Fields                   	| Data type 	| Required / Optional 	| Description                                                            	  |
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
    *  | assetId                 	| String    	| Required            	| BoxId of asset to transfer                                             	  |
    *  | recipient               	| String    	| Required            	| Public key of the transfer recipient                                   	  |
    *  | sender                  	| String[]   	| Required            	| Array of public keys from which assets should be sent                   	|
    *  | amount                  	| Number     	| Required            	| Amount of asset to send                                                	  |
    *  | fee                     	| Number     	| Optional            	| **Currently unused**                                                   	  |
    *  | data                    	| String    	| Optional            	| Data string which can be associated with this transaction (may be empty) 	|
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def transferTargetAssets(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val sender: IndexedSeq[PublicKey25519Proposition] =
        (params \\ "sender").head.asArray.get.map(key =>
          PublicKey25519Proposition(Base58.decode(key.asString.get).get)
        )
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(
        Base58.decode((params \\ "recipient").head.asString.get).get
      )
      val assetId: BoxId = BoxId((params \\ "assetId").head.asString.getOrElse(""))
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val fee: Long =
        (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None          => ""
      }

      val asset = view.state
        .getBox(BoxId(assetId))
        .get
        .asInstanceOf[AssetBox]

      val tx = AssetTransfer
        .create(
          view.state.tbrOpt.get,
          view.state,
          wallet,
          IndexedSeq((recipient, amount)),
          sender,
          fee,
          asset.issuer,
          asset.assetCode,
          data
        )
        .get

      AssetTransfer.semanticValidate(tx, view.state) match {
        case Success(_) =>
          nodeViewHolderRef ! LocallyGeneratedTransaction[AssetTransfer](tx)
          tx.json
        case Failure(e) =>
          throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }

  /** #### Summary
    *   Target a specific asset box and transfer to another account
    * 
    *  #### Type
    *    Remote -- Transaction must be used in conjunction with an external key manager service.
    * 
    *  #### Description
    *    The protocols default behavior is to combine multiple UTXOs of the same type into a single UTXO when it can.
    * 
    *  #### Notes
    *    - Change is returned to the first sender in the array of senders 
    * ---
    *  #### Params
    *
    *  | Fields                   	| Data type 	| Required / Optional 	| Description                                                            	  |
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
    *  | assetId                 	| String    	| Required            	| BoxId of asset to transfer                                             	  |
    *  | recipient               	| String    	| Required            	| Public key of the transfer recipient                                   	  |
    *  | sender                  	| String[]   	| Required            	| Array of public keys from which assets should be sent                   	|
    *  | amount                  	| Number     	| Required            	| Amount of asset to send                                                	  |
    *  | fee                     	| Number     	| Optional            	| **Currently unused**                                                   	  |
    *  | data                    	| String    	| Optional            	| Data string which can be associated with this transaction (may be empty) 	|
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def transferTargetAssetsPrototype(
      params: Json,
      id: String
  ): Future[Json] = {
    viewAsync().map { view =>
      // parse required arguments from the request
      val sender: IndexedSeq[PublicKey25519Proposition] =
        (params \\ "sender").head.asArray.get.map(key =>
          PublicKey25519Proposition(Base58.decode(key.asString.get).get)
        )
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(
        Base58.decode((params \\ "recipient").head.asString.get).get
      )
      val assetId: BoxId = BoxId((params \\ "assetId").head.asString.getOrElse(""))
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)

      // parse optional parameters
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None          => ""
      }

      val asset = view.state
        .getBox(assetId) match {
        case Some(b: AssetBox) => b
        case _ => throw new Error(s"Failed to find specified bow with id: $asset")
      }

      val tx =
        AssetTransfer
          .createRaw(view.state.tbrOpt.get, view.state, IndexedSeq((recipient, amount)), sender, asset.issuer, asset.assetCode, fee, data)
          .get

      AssetTransfer.validatePrototype(tx) match {
        case Success(_) =>
          Map(
            "formattedTx" -> tx.json,
            "messageToSign" -> Base58.encode(tx.messageToSign).asJson
          ).asJson
        case Failure(e) =>
          throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }

  /**  #### Summary
    *    Generate new assets and send them to a specified address.
    * 
    *  #### Type
    *    Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
    * 
    *  #### Description
    *    New boxes will be generated and placed into state under the ownership of the recipient account. Assets are uniquely defined the the combination
    *    of `issuer` and `assetCode`
    * 
    * ---
    *  #### Params
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
    *  | issuer                  	| String    	| Required            	| Asset issuer used to identify asset                                    	  |
    *  | assetCode               	| String    	| Required            	| Name of asset                                                          	  |
    *  | recipient               	| String    	| Required            	| Public key of the transfer recipient                                   	  |
    *  | amount                  	| Number     	| Required            	| Amount of asset to send                                                	  |
    *  | fee                     	| Number     	| Optional            	| **Currently unused**                                                   	  |
    *  | data                    	| String    	| Optional            	| Data string which can be associated with this transaction (may be empty) 	|
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def createAssets(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val issuer = PublicKey25519Proposition(
        Base58.decode((params \\ "issuer").head.asString.get).get
      )
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(
        Base58.decode((params \\ "recipient").head.asString.get).get
      )
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val assetCode: String =
        (params \\ "assetCode").head.asString.getOrElse("")
      val fee: Long =
        (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None          => ""
      }

      val tx =
        AssetCreation
          .createAndApply(wallet, IndexedSeq((recipient, amount)), fee, issuer, assetCode, data)
          .get

      AssetCreation.semanticValidate(tx, view.state) match {
        case Success(_) =>
          nodeViewHolderRef ! LocallyGeneratedTransaction[AssetCreation](tx)
          tx.json
        case Failure(e) =>
          throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }

  /**  #### Summary
    *    Generate new assets and send them to a specified address.
    * 
    *  #### Type
    *    Remote -- Transaction must be used in conjunction with an external key manager service.
    * 
    *  #### Description
    *    New boxes wlll be generated and placed into state under the ownership of the recipient account. Assets are uniquely defined the the combination
    *    of `issuer` and `assetCode`
    * 
    * ---
    *  #### Params
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
    *  | issuer                  	| String    	| Required            	| Asset issuer used to identify asset                                    	  |
    *  | assetCode               	| String    	| Required            	| Name of asset                                                          	  |
    *  | recipient               	| String    	| Required            	| Public key of the transfer recipient                                   	  |
    *  | amount                  	| Number     	| Required            	| Amount of asset to send                                                	  |
    *  | fee                     	| Number     	| Optional            	| **Currently unused**                                                   	  |
    *  | data                    	| String    	| Optional            	| Data string which can be associated with this transaction (may be empty) 	|
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def createAssetsPrototype(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val issuer = PublicKey25519Proposition(
        Base58.decode((params \\ "issuer").head.asString.get).get
      )
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(
        Base58.decode((params \\ "recipient").head.asString.get).get
      )
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val assetCode: String =
        (params \\ "assetCode").head.asString.getOrElse("")
      val fee: Long =
        (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None          => ""
      }
      val tx =
        AssetCreation
          .createPrototype(IndexedSeq((recipient, amount)), fee, issuer, assetCode, data)
          .get

      AssetCreation.validatePrototype(tx) match {
        case Success(_) =>
          Map(
            "formattedTx" -> tx.json,
            "messageToSign" -> Base58.encode(tx.messageToSign).asJson
          ).asJson
        case Failure(e) =>
          throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }
}
