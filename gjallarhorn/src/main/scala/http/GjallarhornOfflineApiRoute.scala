package http

import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter}
import java.time.Instant

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import attestation.{Address, Proposition, PublicKeyPropositionCurve25519, ThresholdPropositionCurve25519}
import attestation.AddressEncoder.NetworkPrefix
import crypto.AssetCode
import http.GjallarhornOfflineApiRoute.updateConfigFile
import io.circe.{HCursor, Json}
import io.circe.syntax._
import keymanager.KeyManager.{ChangeNetwork, GenerateSignatures, GetAllKeyfiles, GetKeyfileDir, SignTx}
import keymanager.networkPrefix
import modifier.{AssetValue, Box, BoxId, SimpleValue, TransferTransaction}
import requests.ApiRoute
import scorex.util.encode.Base58
import settings.{ApplicationSettings, ChainProvider, RPCApiSettings}
import wallet.WalletManager.GetWallet

import scala.collection.mutable.{Map => MMap}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

/**
 * Class route for managing offline requests (bifrost does not need to be running)
 * @param settings - setting for [[ApiRoute]]
 * @param applicationSettings - [[ApplicationSettings]] for communication mode
 * @param keyManagerRef - actor reference for the KeyManager
 * @param walletManagerRef - actor reference for the WalletManager
 * @param context - ActorRef context
 */
case class GjallarhornOfflineApiRoute(
  settings:             RPCApiSettings,
  applicationSettings:  ApplicationSettings,
  keyManagerRef:        ActorRef,
  walletManagerRef:     ActorRef
)(implicit val context: ActorRefFactory)
    extends ApiRoute {

  // Establish the expected network prefix for addresses
  implicit val netPrefix: NetworkPrefix = networkPrefix

  //The namespace for the endpoints defined in handlers
  val namespace: Namespace = WalletNamespace

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    case (method, params, id) if method == s"${namespace.name}_createRawTransaction" =>
      createRawTransaction(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_signTx" => signTx(params.head, id)

    //Get information about state
    case (method, params, id) if method == s"${namespace.name}_balances"        => balances(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_getWalletBoxes"  => getWalletBoxes(id)
    case (method, params, id) if method == s"${namespace.name}_getCurrentState" => getCurrentState(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_networkType" =>
      Future(Map("networkPrefix" -> networkPrefix).asJson)
    case (method, params, id) if method == s"${namespace.name}_getCurrentChainProvider" => getCurrentChainProvider
    case (method, params, id) if method == s"${namespace.name}_getListOfChainProviders" =>
      Future(Map("listOfChainProviders" -> applicationSettings.defaultChainProviders).asJson)

    // Change settings:
    case (method, params, id) if method == s"${namespace.name}_changeNetwork" => changeNetwork(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_changeCurrentChainProvider" =>
      changeCurrentChainProvider(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_addChainProvider"  => addChainProvider(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_editChainProvider" => editChainProvider(params.head, id)
  }

  /**
   * #### Summary
   *    Create raw transaction.
   *
   *  #### Description
   *    Default behavior of the wallet is to find the first unlocked address which hold the targetted asset.
   *    The protocols default behavior is to combine multiple UTXOs of the same type into a single UTXO when it can.
   *
   * ---
   *  #### Params
   *  | Fields    | Data type | Required / Optional | Description                                                |
   *  |-----------|-----------|---------------------|------------------------------------------------------------|
   *  | txType                | String                            | Required | either PolyTransfer, AssetTransfer, or ArbitTransfer
   *  | propositionType       | String                            | Required | PublicKey/ThresholdPropositionCurve25519
   *  | assetCode [if asset transfer]  | String                   | Required | Name of asset                |
   *  | recipients            | IndexedSeq[(Address, AssetValue)] | Required | Recipients and asset values to be sent
   *  | sender                | Address[]                         | Required | Array of public keys from which assets should be sent
   *  | changeAddress         | Address                           | Required  | Address to return change to
   *  | consolidationAddress  | Address                           | Optional  | Address to return leftover arbits or assets to
   *  | fee                   | Number                            | Required  | **Currently unused**
   *  | minting               | Boolean                           | Required  | If new asset creation
   *  | data                  | String                            | Optional  | Data string which can be associated with this transaction(may be empty)|
   *
   * @param params input parameter as specified above
   * @param id request identifier
   * @return
   */
  private def createRawTransaction(params: Json, id: String): Future[Json] = {
    val p = params.hcursor
    (for {
      txType <- p.get[String]("txType")
    } yield txType match {
      case "PolyTransfer"  => rawPolyTransfer(p)
      case "ArbitTransfer" => rawArbitTransfer(p)
      case "AssetTransfer" => rawAssetTransfer(p)
      case _               => throw new Exception(s"Transaction type $txType is not valid")
    }) match {
      case Right(value) => value
      case Left(error)  => throw new Exception(s"error parsing raw tx: $error")
    }
  }

  /**
   * Creates a raw poly transfer
   * @param p - hcursor for transfer params
   * @return raw poly transfer as JSON
   */
  def rawPolyTransfer(p: HCursor): Future[Json] =
    (for {
      propType   <- p.get[String]("propositionType")
      recipients <- p.get[IndexedSeq[(Address, Long)]]("recipients")
      sender     <- p.get[IndexedSeq[Address]]("sender")
      changeAddr <- p.get[Address]("changeAddress")
      fee        <- p.get[Long]("fee")
      data       <- p.get[Option[String]]("data")
    } yield {

      val toReceive = recipients.map(r => r._1 -> SimpleValue(r._2))

      (walletManagerRef ? GetWallet).mapTo[MMap[Address, MMap[BoxId, Box]]].map { walletBoxes =>
        propType match {
          case PublicKeyPropositionCurve25519.`typeString` =>
            TransferTransaction
              .createRawTransferParams(walletBoxes, toReceive, sender, changeAddr, None, fee, "PolyTransfer")
              .map { case (inputs, outputs) =>
                TransferTransaction[PublicKeyPropositionCurve25519](
                  inputs,
                  outputs,
                  Map(),
                  fee,
                  Instant.now.toEpochMilli,
                  data,
                  minting = false,
                  "PolyTransfer"
                )
              } match {
              case Success(tx) => rawTransferResponse(tx)
              case Failure(ex) => throw new Exception(s"Failed to create raw transaction with error: $ex")
            }

          case ThresholdPropositionCurve25519.`typeString` =>
            TransferTransaction
              .createRawTransferParams(walletBoxes, toReceive, sender, changeAddr, None, fee, "PolyTransfer")
              .map { case (inputs, outputs) =>
                TransferTransaction[ThresholdPropositionCurve25519](
                  inputs,
                  outputs,
                  Map(),
                  fee,
                  Instant.now.toEpochMilli,
                  data,
                  minting = false,
                  "PolyTransfer"
                )
              } match {
              case Success(tx) => rawTransferResponse(tx)
              case Failure(ex) => throw new Exception(s"Failed to create raw transaction with error: $ex")
            }
        }
      }

    }) match {
      case Right(value) => value
      case Left(error)  => throw new Exception(s"error parsing raw tx: $error")
    }

  /**
   * Creates a raw arbit transfer
   * @param p - hcursor for transfer params
   * @return raw arbit transfer as JSON
   */
  private[http] def rawArbitTransfer(p: HCursor): Future[Json] =
    (for {
      propType          <- p.get[String]("propositionType")
      recipients        <- p.get[IndexedSeq[(Address, Long)]]("recipients")
      sender            <- p.get[IndexedSeq[Address]]("sender")
      changeAddr        <- p.get[Address]("changeAddress")
      consolidationAddr <- p.get[Option[Address]]("consolidationAddress")
      fee               <- p.get[Long]("fee")
      data              <- p.get[Option[String]]("data")
    } yield {

      val toReceive = recipients.map(r => r._1 -> SimpleValue(r._2))

      (walletManagerRef ? GetWallet).mapTo[MMap[Address, MMap[BoxId, Box]]].map { walletBoxes =>
        propType match {
          case PublicKeyPropositionCurve25519.`typeString` =>
            TransferTransaction
              .createRawTransferParams(
                walletBoxes,
                toReceive,
                sender,
                changeAddr,
                consolidationAddr,
                fee,
                "ArbitTransfer"
              )
              .map { case (inputs, outputs) =>
                TransferTransaction[PublicKeyPropositionCurve25519](
                  inputs,
                  outputs,
                  Map(),
                  fee,
                  Instant.now.toEpochMilli,
                  data,
                  minting = false,
                  "ArbitTransfer"
                )
              } match {
              case Success(tx) => rawTransferResponse(tx)
              case Failure(ex) => throw new Exception(s"Failed to create raw transaction with error: $ex")
            }

          case ThresholdPropositionCurve25519.`typeString` =>
            TransferTransaction
              .createRawTransferParams(
                walletBoxes,
                toReceive,
                sender,
                changeAddr,
                consolidationAddr,
                fee,
                "ArbitTransfer"
              )
              .map { case (inputs, outputs) =>
                TransferTransaction[ThresholdPropositionCurve25519](
                  inputs,
                  outputs,
                  Map(),
                  fee,
                  Instant.now.toEpochMilli,
                  data,
                  minting = false,
                  "ArbitTransfer"
                )
              } match {
              case Success(tx) => rawTransferResponse(tx)
              case Failure(ex) => throw new Exception(s"Failed to create raw transaction with error: $ex")
            }
        }
      }

    }) match {
      case Right(value) => value
      case Left(error)  => throw new Exception(s"error parsing raw tx: $error")
    }

  /**
   * Creates a raw asset transfer
   * @param p - hcursor for transfer params
   * @return raw asset transfer as JSON
   */
  private[http] def rawAssetTransfer(p: HCursor): Future[Json] =
    (for {
      propType          <- p.get[String]("propositionType")
      recipients        <- p.get[IndexedSeq[(Address, Long)]]("recipients")
      issuer            <- p.get[Address]("issuer")
      shortName         <- p.get[String]("shortName")
      sender            <- p.get[IndexedSeq[Address]]("sender")
      changeAddr        <- p.get[Address]("changeAddress")
      consolidationAddr <- p.get[Option[Address]]("consolidationAddress")
      minting           <- p.get[Boolean]("minting")
      fee               <- p.get[Long]("fee")
      data              <- p.get[Option[String]]("data")
    } yield {

      //TODO: what should assetCode version be?
      val assetCode = Try(AssetCode(1.toByte, issuer, shortName)) match {
        case Success(code) => code
        case Failure(ex)   => throw new Exception(s"Unable to generate asset code: $ex")
      }

      val toReceive = recipients.map(r => r._1 -> AssetValue(r._2, assetCode))

      (walletManagerRef ? GetWallet).mapTo[MMap[Address, MMap[BoxId, Box]]].map { walletBoxes =>
        propType match {
          case PublicKeyPropositionCurve25519.`typeString` =>
            TransferTransaction
              .createRawTransferParams(
                walletBoxes,
                toReceive,
                sender,
                changeAddr,
                consolidationAddr,
                fee,
                "AssetTransfer"
              )
              .map { case (inputs, outputs) =>
                TransferTransaction[PublicKeyPropositionCurve25519](
                  inputs,
                  outputs,
                  Map(),
                  fee,
                  Instant.now.toEpochMilli,
                  data,
                  minting,
                  "AssetTransfer"
                )
              } match {
              case Success(tx) => rawTransferResponse(tx)
              case Failure(ex) => throw new Exception(s"Failed to create raw transaction with error: $ex")
            }

          case ThresholdPropositionCurve25519.`typeString` =>
            TransferTransaction
              .createRawTransferParams(
                walletBoxes,
                toReceive,
                sender,
                changeAddr,
                consolidationAddr,
                fee,
                "AssetTransfer"
              )
              .map { case (inputs, outputs) =>
                TransferTransaction[ThresholdPropositionCurve25519](
                  inputs,
                  outputs,
                  Map(),
                  fee,
                  Instant.now.toEpochMilli,
                  data,
                  minting,
                  "AssetTransfer"
                )
              } match {
              case Success(tx) => rawTransferResponse(tx)
              case Failure(ex) => throw new Exception(s"Failed to create raw transaction with error: $ex")
            }
        }
      }

    }) match {
      case Right(value) => value
      case Left(error)  => throw new Exception(s"error parsing raw tx: $error")
    }

  /**
   * Helper function for creating API response to a raw transfer request
   * @param tx - the raw transaction to be sent back
   * @return JSON mapping of rawTx and messageToSign
   */
  def rawTransferResponse(tx: TransferTransaction[_ <: Proposition]): Json =
    tx.rawValidate match {
      case Success(_) =>
        Map(
          "rawTx"         -> tx.asJson,
          "messageToSign" -> Base58.encode(tx.messageToSign).asJson
        ).asJson

      case Failure(exception) => throw new Exception(s"Could not validate transaction: $exception")
    }

  /**
   * #### Summary
   * Sign transaction
   *
   * #### Description
   * Signs a transaction - adds a signature to a raw transaction.
   * ---
   * #### Params
   *
   * | Fields | Data type | Required / Optional | Description |
   * | ---| --- | --- | --- |
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
      signingKeys   <- (params \\ "signingKeys").head.as[IndexedSeq[Address]]
      messageToSign <- (params \\ "messageToSign").head.as[String]
    } yield
      if (tx.asObject.isDefined) {
        (keyManagerRef ? SignTx(tx, signingKeys, messageToSign)).mapTo[Json]
      } else {
        (keyManagerRef ? GenerateSignatures(signingKeys, messageToSign)).mapTo[Json]
      }) match {
      case Right(value) => value
      case Left(error)  => throw new Exception(s"error parsing signing keys: $error")
    }
  }

  /**
   * #### Summary
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
  private def changeNetwork(params: Json, id: String): Future[Json] =
    (for {
      newNetwork <- (params \\ "newNetwork").head.as[String]
    } yield (keyManagerRef ? ChangeNetwork(newNetwork)).mapTo[Try[Json]].map {
      case Success(value) => value
      case Failure(ex)    => throw ex
    }) match {
      case Right(value) => value
      case Left(error)  => throw new Exception(s"error parsing new network: $error")
    }

  /**
   * #### Summary
   * Get Wallet Boxes
   *
   * #### Description
   * Returns the current wallet boxes for the running wallet.
   * ---
   * #### Params
   * | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
   * | ------------------------	| ----------	| --------------------	| -----------------------------------------------------------------------	  |
   * | --None specified--       |           	|                     	|                                                                         |
   *
   * @param id     request identifier
   * @return - wallet boxes
   */
  private def getWalletBoxes(id: String): Future[Json] =
    (walletManagerRef ? GetWallet).mapTo[MMap[Address, MMap[BoxId, Box]]].map(_.asJson)

  /**
   * #### Summary
   * Lookup balances
   *
   * #### Description
   * Returns balances for specified keys (or all of the keys in the wallet) based on the wallet boxes in the WalletManager.
   *
   * #### Params
   * | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
   * |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
   * | publicKeys               | String[]   	| Optional            	| Public keys whose balances are to be retrieved                            	|
   *
   * @param params input parameters as specified above
   * @param id     request identifier
   * @return - mapping of balances (ArbitBox -> #, PolyBox -> #, assetcode -> #)
   */
  private def balances(params: Json, id: String): Future[Json] =
    (walletManagerRef ? GetWallet).mapTo[MMap[Address, MMap[BoxId, Box]]].map { walletResponse =>
      var publicKeys: Set[Address] = walletResponse.keySet.toSet
      if ((params \\ "addresses").nonEmpty) {
        publicKeys = (params \\ "addresses").head.asArray.get.map(k => Address(networkPrefix)(k.asString.get)).toSet
      }
      val balances: MMap[Address, MMap[String, Long]] = MMap.empty
      publicKeys.foreach { addr =>
        val getBoxes: Option[MMap[BoxId, Box]] = walletResponse.get(addr)
        var assets: MMap[String, Long] = MMap.empty
        getBoxes match {
          case Some(boxes) =>
            var polyBalance: Long = 0
            var arbitBalance: Long = 0
            val assetBalance: MMap[String, Long] = MMap.empty
            boxes.foreach { box =>
              box._2.typeOfBox match {
                case "ArbitBox" => arbitBalance = arbitBalance + box._2.value.quantity
                case "PolyBox"  => polyBalance = polyBalance + box._2.value.quantity
                case "AssetBox" =>
                  val assetValue = box._2.value.asInstanceOf[AssetValue]
                  val assetCode = assetValue.assetCode
                  assetBalance.get(assetCode.toString) match {
                    case Some(oldBalance) =>
                      val newBalance = oldBalance + assetValue.quantity
                      assetBalance.put(assetCode.toString, newBalance)
                    case None =>
                      assetBalance.put(assetCode.toString, assetValue.quantity)
                  }
              }
            }
            assets = MMap(
              "ArbitBox" -> arbitBalance,
              "PolyBox"  -> polyBalance
            ) ++ assetBalance
          case None => null
        }
        balances.put(addr, assets)
      }
      balances.asJson
    }

  /**
   * #### Summary
   * Change the current chain provider
   *
   * #### Description
   * Changes the current chain provider to the given chain provider.
   * Either an AkkaChainProvider or an HttpChainProvider
   * ---
   * #### Params
   *
   * | Fields | Data type | Required / Optional | Description |
   * | ---| ---	| --- | --- |
   * | chainProvider | ChainProvider	| Required | the new chainProvider to switch to |
   *
   * @param params input parameters as specified above
   * @param id     request identifier
   * @return - json mapping: "currentChainProvider" -> applicationSettings.currentChainProvider
   */
  private def changeCurrentChainProvider(params: Json, id: String): Future[Json] =
    (for {
      chainProvider <- (params \\ "chainProvider").head.as[ChainProvider]
    } yield {
      val newName = chainProvider.name
      applicationSettings.defaultChainProviders.get(newName) match {
        case Some(_) =>
          updateConfigFile("current-chain-provider", applicationSettings.currentChainProvider, newName)
          applicationSettings.currentChainProvider = newName
          Map("currentChainProvider" -> applicationSettings.currentChainProvider).asJson
        case None =>
          throw new Exception(
            s"The new chain provider: $newName does not map to a chain provider in " +
            s"the list of chain providers."
          )
      }

    }) match {
      case Right(value) => Future(value)
      case Left(error)  => throw new Exception(s"error parsing for mode: $error")
    }

  /**
   * #### Summary
   * Add a chain provider to the list of chain providers
   *
   * #### Description
   * Adds a chain provider to the current list of chain providers.
   * *But does not write to application.conf file -> does not add to default list.
   * ---
   * #### Params
   *
   * | Fields | Data type | Required / Optional | Description |
   * | ---| ---	| --- | --- |
   * | chainProvider | ChainProvider	| Required | the new chainProvider to add|
   *
   * @param params input parameters as specified above
   * @param id request identifier
   * @return a map of the chain provider's name to "Added"
   */
  private def addChainProvider(params: Json, id: String): Future[Json] =
    (for {
      chainProvider <- (params \\ "chainProvider").head.as[ChainProvider]
    } yield {
      val currentList = collection.mutable.Map(applicationSettings.defaultChainProviders.toSeq: _*)
      currentList.get(chainProvider.name) match {
        case Some(_) => throw new Exception(s"A chain provider with this name: ${chainProvider.name} already exists!")
        case None =>
          currentList.put(chainProvider.name, chainProvider)
          applicationSettings.defaultChainProviders = currentList.toMap
          Future(Map(chainProvider.name -> "Added").asJson)
      }
    }) match {
      case Right(value) => value
      case Left(error)  => throw new Exception(s"error parsing for mode: $error")
    }

  /**
   * #### Summary
   * Edits a chain provider in the list of chain providers
   *
   * #### Description
   * Replaces an editted chain provider in the list of chain providers
   * * These changes are not writting to application.conf so are not saved after terminating the app.
   * ---
   * #### Params
   *
   * | Fields | Data type | Required / Optional | Description |
   * | ---| ---	| --- | --- |
   * | chainProvider | ChainProvider	| Required | the edited chainProvider |
   *
   * @param params input parameters as specified above
   * @param id request identifier
   * @return a map of the chain provider's name to "Edited"
   */
  private def editChainProvider(params: Json, id: String): Future[Json] =
    (for {
      chainProvider <- (params \\ "chainProvider").head.as[ChainProvider]
    } yield {
      val currentList = collection.mutable.Map(applicationSettings.defaultChainProviders.toSeq: _*)
      currentList.get(chainProvider.name) match {
        case Some(_) =>
          currentList.put(chainProvider.name, chainProvider)
          applicationSettings.defaultChainProviders = currentList.toMap
          Future(Map(chainProvider.name -> "Edited").asJson)
        case None => throw new Exception(s"A chain provider with this name: ${chainProvider.name} does not exist!")
      }
    }) match {
      case Right(value) => value
      case Left(error)  => throw new Exception(s"error parsing for chainProvider: $error")
    }

  /**
   * #### Summary
   * Returns information about the current state.
   *
   * #### Description
   * Returns current state information: networkPrefix, currentChainProvider, listOfChainProviders, keyfileDirectory, and keys.
   * ---
   * #### Params
   *
   * | Fields | Data type | Required / Optional | Description |
   * | ---| ---	| --- | --- |
   * | --None specified-- |         	|                     	|
   *
   * @param params input parameters as specified above
   * @param id     request identifier
   * @return - json mapping of state information
   */
  private def getCurrentState(params: Json, id: String): Future[Json] =
    Await.result(
      (keyManagerRef ? GetKeyfileDir).mapTo[Json].map { value =>
        val directory = (value \\ "keyfileDirectory").head
        Await.result(
          (keyManagerRef ? GetAllKeyfiles).mapTo[Map[Address, String]].map { keys =>
            Future {
              Map(
                "networkPrefix"        -> networkPrefix.asJson,
                "currentChainProvider" -> applicationSettings.currentChainProvider.asJson,
                "listOfChainProviders" -> applicationSettings.defaultChainProviders.asJson,
                "keyfileDirectory"     -> directory,
                "keys"                 -> keys.asJson
              ).asJson
            }
          },
          10.seconds
        )
      },
      10.seconds
    )

  private def getCurrentChainProvider: Future[Json] = {
    val current = applicationSettings.currentChainProvider
    applicationSettings.defaultChainProviders.get(current) match {
      case Some(cp) => Future(Map("currentChainProvider" -> cp).asJson)
      case None =>
        throw new Exception(
          s"The current chain provider: $current does not map to a chain provider in " +
          s"the list of chain providers."
        )
    }
  }

}

object GjallarhornOfflineApiRoute {

  /**
   * Changes a setting value within the "application.conf" file given the old value and the new value.
   * @param settingName the setting to change
   * @param oldValue the old value of the given setting
   * @param newValue the new value for the given setting
   */
  def updateConfigFile(settingName: String, oldValue: String, newValue: String): Unit = {
    //grab config file to write to
    //TODO: should this path be hardcoded?
    val path = "gjallarhorn/src/main/resources/application.conf"
    val configFile: File = new File(path)
    if (!configFile.exists()) {
      throw new Error(s"The config file: $path does not exist!")
    }

    //find line that defines settingName and edit it so that it is set to the new value
    var lines: Array[String] = Array.empty
    val reader = new BufferedReader(new FileReader(configFile))
    var line: String = ""
    while ({ line = reader.readLine; line != null })
      if (line.contains(settingName)) {
        var newLine = line
        if (oldValue == "") {
          newLine = line.substring(0, line.indexOf("=") + 1) + s""" "$newValue""""
        } else {
          newLine = line.replace(oldValue, newValue)
        }
        lines = lines :+ newLine
      } else {
        lines = lines :+ line
      }
    reader.close()

    val writer = new BufferedWriter(new FileWriter(configFile))
    lines.foreach { line =>
      writer.write(line)
      writer.newLine()
    }
    writer.close()
  }

}
