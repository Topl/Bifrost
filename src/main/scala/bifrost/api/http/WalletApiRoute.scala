package bifrost.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.history.History
import bifrost.mempool.MemPool
import bifrost.state.State
import bifrost.modifier.box.Box
import bifrost.wallet.Wallet
import bifrost.network.BifrostLocalInterface.LocallyGeneratedTransaction
import bifrost.crypto.{Bip39, PrivateKey25519}
import bifrost.settings.Settings
import bifrost.modifier.transaction.bifrostTransaction._
import bifrost.modifier.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import scorex.crypto.encode.Base58

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

case class WalletApiRoute(override val settings: Settings, nodeViewHolderRef: ActorRef)
                         (implicit val context: ActorRefFactory)extends ApiRouteWithView {
  type HIS = History
  type MS = State
  type VL = Wallet
  type MP = MemPool
  override val route: Route = pathPrefix("wallet") {
    walletRoute
  }

  def walletRoute: Route = path("") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          viewAsync().map { view =>
            var reqId = ""
            parse(body) match {
              case Left(failure) => ErrorResponse(failure.getCause, 400, reqId)
              case Right(request) =>
                val futureResponse: Try[Future[Json]] = Try {
                  val id = (request \\ "id").head.asString.get
                  reqId = id
                  require((request \\ "jsonrpc").head.asString.get == "2.0")
                  val params = (request \\ "params").head.asArray.get
                  //todo: why is there an enforcement on the size of params?
                  require(params.size <= 5, s"size of params is ${params.size}")

                  (request \\ "method").head.asString.get match {
                    case "transferPolys"  => transferPolys(params.head, id)
                    case "transferArbits" => transferArbits(params.head, id)
                    case "transferArbitsPrototype" => transferArbitsPrototype(params.head, id)
                    case "transferPolysPrototype" => transferPolysPrototype(params.head, id)
                    case "balances"         => balances(params.head, id)
                    case "unlockKeyfile"    => unlockKeyfile(params.head, id)
                    case "lockKeyfile"      => lockKeyfile(params.head, id)
                    case "generateKeyfile"  => generateKeyfile(params.head, id)
                    case "listOpenKeyfiles" => listOpenKeyfiles(params.head, id)
                    case "importSeedPhrase" => importKeyfile(params.head, id)
                    case "signTx"           => signTx(params.head, id)
                    case "broadcastTx"      => broadcastTx(params.head, id)
                  }
                }
                futureResponse map { response =>
                  Await.result(response, timeout.duration)
                } match {
                  case Success(resp) => SuccessResponse(resp, reqId)
                  case Failure(e) =>
                    ErrorResponse(
                      e,
                      500,
                      reqId,
                      verbose = settings.settingsJSON
                        .getOrElse("verboseAPI", false.asJson)
                        .asBoolean
                        .get
                    )
                }
            }
          }
        }
      }
    }
  }

  /**  #### Summary
    *    Transfer Polys from an account to a specified recipient.
    * 
    *  #### Type
    *    Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
    * 
    *  #### Description
    *    Default behavior of the wallet is to find the first unlocked address which hold Polys.
    *    The protocols default behavior is to combine multiple UTXOs of the same type into a single UTXO when it can.
    * 
    *  #### Notes
    *    - Change is returned to the first sender in the array of senders
    *  ---
    *  #### Params
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
    *  | recipient               	| String    	| Required            	| Public key of the transfer recipient                                   	  |
    *  | sender                  	| String[]   	| Required            	| Array of public keys from which assets should be sent                   	|
    *  | amount                  	| Number     	| Required            	| Amount of asset to send                                                	  |
    *  | fee                     	| Number     	| Optional            	| Default to 0                                                          	  |
    *  | data                    	| String    	| Optional            	| Data string which can be associated with this transaction (may be empty) 	|
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def transferPolys(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(
        Base58.decode((params \\ "recipient").head.asString.get).get
      )
      val sender: IndexedSeq[PublicKey25519Proposition] =
        (params \\ "sender").head.asArray.get
          .map(key =>
            PublicKey25519Proposition(Base58.decode(key.asString.get).get)
          )
          .toIndexedSeq
      val fee: Long =
        (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      // Optional API parameters
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None          => ""
      }
      if (view.state.tbr == null)
        throw new Exception("TokenBoxRegistry not defined for node")
      //YT NOTE - if nodeKeys not defined in settings file then node watches for all keys in a state update
      if (view.state.nodeKeys != null)
        sender.foreach(key =>
          if (!view.state.nodeKeys.contains(ByteArrayWrapper(key.pubKeyBytes)))
            throw new Exception(
              "Node not set to watch for specified public key"
            )
        )
      // Call to BifrostTX to create TX
      val tx = PolyTransfer
        .create(
          view.state.tbr,
          wallet,
          IndexedSeq((recipient, amount)),
          sender,
          fee,
          data
        )
        .get
      // Update nodeView with new TX
      PolyTransfer.validate(tx) match {
        case Success(_) =>
          nodeViewHolderRef ! LocallyGeneratedTransaction[
            ProofOfKnowledgeProposition[PrivateKey25519],
            PolyTransfer
          ](tx)
          tx.json
        case Failure(e) =>
          throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }

  /**  #### Summary
    *    Transfer Polys from an account to a specified recipient.
    * 
    *  #### Type
    *    Remote -- Transaction must be used in conjunction with an external key manager service.
    * 
    *  #### Description
    *    Default behavior of the wallet is to find the first unlocked address which hold Polys.
    *    The protocols default behavior is to combine multiple UTXOs of the same type into a single UTXO when it can.
    * 
    *  #### Notes
    *    - Change is returned to the first sender in the array of senders
    *  ---
    *  #### Params
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
    *  | recipient               	| String    	| Required            	| Public key of the transfer recipient                                   	  |
    *  | sender                  	| String[]   	| Required            	| Array of public keys from which assets should be sent                   	|
    *  | amount                  	| Number    	| Required            	| Amount of asset to send                                                	  |
    *  | fee                     	| Number     	| Optional            	| Default to 0                                                          	  |
    *  | data                    	| String    	| Optional            	| Data string which can be associated with this transaction (may be empty) 	|
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def transferPolysPrototype(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(
        Base58.decode((params \\ "recipient").head.asString.get).get
      )
      val sender: IndexedSeq[PublicKey25519Proposition] =
        (params \\ "sender").head.asArray.get
          .map(key =>
            PublicKey25519Proposition(Base58.decode(key.asString.get).get)
          )
          .toIndexedSeq
      val fee: Long =
        (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      // Optional API parameters
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None          => ""
      }

      if (view.state.tbr == null)
        throw new Exception("TokenBoxRegistry not defined for node")
      if (view.state.nodeKeys != null)
        sender.foreach(key =>
          if (!view.state.nodeKeys.contains(ByteArrayWrapper(key.pubKeyBytes)))
            throw new Exception(
              "Node not set to watch for specified public key"
            )
        )
      val tx = PolyTransfer
        .createPrototype(
          view.state.tbr,
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

  /**  #### Summary
    *    Transfer Arbits from an account to a specified recipient.
    * 
    *  #### Type
    *    Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
    * 
    *  #### Description
    *    Default behavior of the wallet is to find the first unlocked address which hold Arbits.
    *    The protocols default behavior is to combine multiple UTXOs of the same type into a single UTXO when it can.
    * 
    *  #### Notes
    *    - Change is returned to the first sender in the array of senders
    *  ---
    *  #### Params
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
    *  | recipient               	| String    	| Required            	| Public key of the transfer recipient                                   	  |
    *  | sender                  	| String[]   	| Required            	| Array of public keys from which assets should be sent                   	|
    *  | amount                  	| Number     	| Required            	| Amount of asset to send                                                	  |
    *  | fee                     	| Number     	| Optional            	| Default to 0                                                          	  |
    *  | data                    	| String    	| Optional            	| Data string which can be associated with this transaction (may be empty) 	|
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def transferArbits(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(
        Base58.decode((params \\ "recipient").head.asString.get).get
      )
      val sender: IndexedSeq[PublicKey25519Proposition] =
        (params \\ "sender").head.asArray.get
          .map(key =>
            PublicKey25519Proposition(Base58.decode(key.asString.get).get)
          )
          .toIndexedSeq
      val fee: Long =
        (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      // Optional API parameters
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None          => ""
      }
      if (view.state.tbr == null)
        throw new Exception("TokenBoxRegistry not defined for node")
      if (view.state.nodeKeys != null)
        sender.foreach(key =>
          if (!view.state.nodeKeys.contains(ByteArrayWrapper(key.pubKeyBytes)))
            throw new Exception(
              "Node not set to watch for specified public key"
            )
        )
      val tx = ArbitTransfer
        .create(
          view.state.tbr,
          wallet,
          IndexedSeq((recipient, amount)),
          sender,
          fee,
          data
        )
        .get
      // Update nodeView with new TX
      ArbitTransfer.validate(tx) match {
        case Success(_) =>
          nodeViewHolderRef ! LocallyGeneratedTransaction[
            ProofOfKnowledgeProposition[PrivateKey25519],
            ArbitTransfer
          ](tx)
          tx.json
        case Failure(e) =>
          throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }

  /**  #### Summary
    *    Transfer Polys from an account to a specified recipient.
    * 
    *  #### Type
    *    Remote -- Transaction must be used in conjunction with an external key manager service.
    * 
    *  #### Description
    *    Default behavior of the wallet is to find the first unlocked address which hold Arbits.
    *    The protocols default behavior is to combine multiple UTXOs of the same type into a single UTXO when it can.
    * 
    *  #### Notes
    *    - Change is returned to the first sender in the array of senders
    *  ---
    *  #### Params
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
    *  | recipient               	| String    	| Required            	| Public key of the transfer recipient                                   	  |
    *  | sender                  	| String[]   	| Required            	| Array of public keys from which assets should be sent                   	|
    *  | amount                  	| Number     	| Required            	| Amount of asset to send                                                	  |
    *  | fee                     	| Number     	| Optional            	| Default to 0                                                          	  |
    *  | data                    	| String    	| Optional            	| Data string which can be associated with this transaction (may be empty) 	|
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def transferArbitsPrototype(
      params: Json,
      id: String
  ): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(
        Base58.decode((params \\ "recipient").head.asString.get).get
      )
      val sender: IndexedSeq[PublicKey25519Proposition] =
        (params \\ "sender").head.asArray.get
          .map(key =>
            PublicKey25519Proposition(Base58.decode(key.asString.get).get)
          )
          .toIndexedSeq
      val fee: Long =
        (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      // Optional API parameters
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None          => ""
      }

      if (view.state.tbr == null)
        throw new Exception("TokenBoxRegistry not defined for node")
      if (view.state.nodeKeys != null)
        sender.foreach(key =>
          if (!view.state.nodeKeys.contains(ByteArrayWrapper(key.pubKeyBytes)))
            throw new Exception(
              "Node not set to watch for specified public key"
            )
        )
      val tx = ArbitTransfer
        .createPrototype(
          view.state.tbr,
          IndexedSeq((recipient, amount)),
          sender,
          fee,
          data
        )
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

  /**  #### Summary
    *    Lookup balances
    * 
    *  #### Type
    *    Remote -- Transaction must be used in conjunction with an external key manager service.
    * 
    *  #### Description
    *    Check balances of specified keys.
    * 
    *  #### Notes
    *    - Requires the Token Box Registry to be active
    *  ---
    *  #### Params
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
    *  | publicKey              	| String[]   	| Required            	| Public key whose balances are to be retrieved                            	|
    * 
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def balances(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val tbr = view.state.tbr
      val publicKeys = (params \\ "publicKeys").head.asArray.get.map(k =>
        PublicKey25519Proposition(Base58.decode(k.asString.get).get)
      )
      val boxes: Map[PublicKey25519Proposition, Map[String, Seq[Box]]] =
        publicKeys
          .map(k => k -> tbr.boxesByKey(k).groupBy[String](_.typeOfBox))
          .toMap
      val balances: Map[PublicKey25519Proposition, (String, String)] =
        boxes.map {
          case (prop, boxes) =>
            val sums = (
              if (boxes.contains("Poly"))
                boxes("Poly")
                  .foldLeft(0L)((a, b) => a + b.value.asInstanceOf[Long])
                  .toString
              else "0",
              if (boxes.contains("Arbit"))
                boxes("Arbit")
                  .foldLeft(0L)((a, b) => a + b.value.asInstanceOf[Long])
                  .toString
              else "0"
            )
            prop -> sums
        }

      boxes.map {
        case (prop, boxes) =>
          Base58.encode(prop.pubKeyBytes) -> Map(
            "Balances" -> Map(
              "Polys" -> balances(prop)._1,
              "Arbits" -> balances(prop)._2
            ).asJson,
            "Boxes" -> boxes.map(b => b._1 -> b._2.map(_.json).asJson).asJson
          )
      }.asJson

    }
  }

  /**  #### Summary
    *    Generate a new keyfile in local storage
    * 
    *  #### Type
    *    Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
    * 
    *  #### Description
    *    Generate and save a new encrypted private keyfile using Curve25519 key pairs.
    *  ---
    *  #### Params
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
    *  | password                	| String    	| Required            	| String used to encrypt the private keyfile that is stored locally        	|
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def generateKeyfile(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val password: String = (params \\ "password").head.asString.get
      val pubKey = wallet.generateNewSecret(password)
      Map(
        "publicKey" -> Base58.encode(pubKey.pubKeyBytes).asJson
      ).asJson
    }
  }

  /**  #### Summary
    *    Import key from mnemonic
    * 
    *  #### Type
    *    Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
    * 
    *  #### Description
    *    Allows a user to import a 12, 15, 18, 21, or 24 word mnemonic (seed phrase) and generate an encrypted Keyfile
    *  ---
    *  #### Params
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
    *  | password                	| String    	| Required            	| String used to encrypt the private keyfile that is stored locally         |
    *  | seedPhrase              	| String    	| Required            	| 12, 15, 18, 21, or 24 word mnemonic							         	                |
    *  | seddPhraseLang         	| String    	| Optional            	| Defaults to 'en'. Valid options are ["zh-hans", "zh-hant", "en", "fr", "it", "ja", "ko", "es"] |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def importKeyfile(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val password: String = (params \\ "password").head.asString.get
      val seedPhrase: String = (params \\ "seedPhrase").head.asString.get
      val seedPhraseLang: String =
        (params \\ "seedPhraseLang").headOption match {
          case Some(dataStr) => dataStr.asString.getOrElse("")
          case None          => "en"
        }
      val pt = Bip39.apply(seedPhraseLang)
      Map(
        pt.phraseCheckSum(seedPhrase) match {
          case false => "error:" -> "not a valid input phrase".asJson
          case true => {
            val seed = pt.hexToUuid(pt.phraseToHex(seedPhrase))
            val pubKey = wallet.generateNewSecret(password, seed)
            "publicKey" -> Base58.encode(pubKey.pubKeyBytes).asJson
          }

        }
      ).asJson
    }
  }

  /**  #### Summary
    *    Unlock keyfile
    * 
    *  #### Type
    *    Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
    * 
    *  #### Description
    *    Unlock an encrypted keyfile which exists in your keyfile directory. This will add the secret key to wallet and allow signing of transactions on behalf of that key
    *  ---
    *  #### Params
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	    |
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	    |
    *  | publicKey               	| String    	| Required            	| Public key corresponding to an encrypted keyfile in your wallet directory   |
    *  | password                	| String    	| Required            	| String used to encrypt the private keyfile that is stored locally        	  |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def unlockKeyfile(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val publicKey: String = (params \\ "publicKey").head.asString.get
      val password: String = (params \\ "password").head.asString.get
      wallet.unlockKeyFile(publicKey, password)
      Map(
        publicKey -> "unlocked".asJson
      ).asJson
    }
  }

  /**  #### Summary
    *    Lock keyfile
    * 
    *  #### Type
    *    Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
    * 
    *  #### Description
    *    Lock a previously unlocked keyfile in your wallet.
    *  ---
    *  #### Params
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
    *  | publicKey               	| String    	| Required            	| Public key corresponding to an encrypted keyfile in your wallet directory |
    *  | password                	| String    	| Required            	| String used to encrypt the private keyfile that is stored locally         |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def lockKeyfile(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val publicKey: String = (params \\ "publicKey").head.asString.get
      val password: String = (params \\ "password").head.asString.get
      wallet.lockKeyFile(publicKey, password)
      Map(
        publicKey -> "locked".asJson
      ).asJson
    }
  }

  /**  #### Summary
    *    Return list of open keyfiles
    * 
    *  #### Type
    *    Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
    * 
    *  #### Description
    *    Check which keyfiles are currently unlocked in your wallet. This method takes no input arguments.
    *  ---
    *  #### Params
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
    *  | --None specified--       |           	|                     	|                                                                         |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def listOpenKeyfiles(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      wallet.secrets
        .flatMap(_ match {
          case pkp: PrivateKey25519 => Some(Base58.encode(pkp.publicKeyBytes))
          case _                    => None
        })
        .asJson
    }
  }

  /**  #### Summary
    *    Sign a prototype transaction
    * 
    *  #### Type
    *    Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
    * 
    *  #### Description
    *    Generate and return a signature attached to the specified transaction
    * 
    *  #### Notes
    *    - Currently only enabled for `AssetCreation` and `AssetTransfer` transactions
    *  ---
    *  #### Params
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
    *  | signingKeys             	| String[]   	| Required            	| List of keys that signatures will be created for                       	  |
    *  | protoTx             	    | object     	| Required            	| A prototype transaction JSON object                                     	|
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def signTx(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val props = (params \\ "signingKeys").head.asArray.get.map(k =>
        PublicKey25519Proposition(Base58.decode(k.asString.get).get)
      )
      val tx = (params \\ "protoTx").head
      val txType = (tx \\ "txType").head.asString.get
      val txInstance = txType match {
        case "AssetCreation" => tx.as[AssetCreation].right.get
        case "AssetTransfer" => tx.as[AssetTransfer].right.get
        case _ =>
          throw new Exception(s"Could not find valid transaction type $txType")
      }
      val signatures: Json = Map(
        "signatures" -> BifrostTransaction
          .signTx(wallet, props, txInstance.messageToSign)
          .map(sig => {
            Base58.encode(sig._1.pubKeyBytes) -> Base58
              .encode(sig._2.signature)
              .asJson
          })
      ).asJson

      tx.deepMerge(signatures)
    }
  }

  /**  #### Summary
    *    Broadcast transaction
    * 
    *  #### Type
    *    Remote -- Route must be used in conjunction with an external key manager service.
    * 
    *  #### Description
    *    Place specified signed transaction into the mempool and broadcast to other nodes
    * 
    *  #### Notes
    *    - Currently only enabled for `AssetCreation` and `AssetTransfer` transactions
    *  ---
    *  #### Params
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	      |
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	      |
    *  | tx                 	    | object     	| Required            	| A full formatted transaction JSON object (prototype transaction + signatures) |
    * 
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def broadcastTx(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val tx = (params \\ "tx").head
      val txType = (tx \\ "txType").head.asString.get
      val txInstance: BifrostTransaction = txType match {
        case "AssetCreation" => tx.as[AssetCreation].right.get
        case "AssetTransfer" => tx.as[AssetTransfer].right.get
        case _ =>
          throw new Exception(s"Could not find valid transaction type $txType")
      }

      view.state.semanticValidity(txInstance)
      nodeViewHolderRef ! LocallyGeneratedTransaction[
        ProofOfKnowledgeProposition[PrivateKey25519],
        BifrostTransaction
      ](txInstance)
      txInstance.json
    }
  }
}
