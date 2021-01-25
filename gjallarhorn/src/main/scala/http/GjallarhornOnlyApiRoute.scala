package http

import java.time.Instant

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import attestation.{Address, PublicKeyPropositionCurve25519, ThresholdPropositionCurve25519}
import attestation.AddressEncoder.NetworkPrefix
import crypto.AssetCode
import io.circe.{HCursor, Json}
import io.circe.syntax._
import keymanager.KeyManager.{ChangeNetwork, GenerateSignatures, SignTx}
import keymanager.networkPrefix
import modifier.{AssetValue, Box, BoxId, SimpleValue, TransferTransaction}
import requests.{ApiRoute, Requests}
import scorex.util.encode.Base58
import settings.AppSettings
import wallet.WalletManager.GetWallet

import scala.collection.mutable.{Map => MMap}
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

case class GjallarhornOnlyApiRoute (settings: AppSettings,
                                    keyManagerRef: ActorRef,
                                    walletManagerRef: ActorRef,
                                    requests: Requests)
                                   (implicit val context: ActorRefFactory)
  extends ApiRoute {

  val namespace: Namespace = WalletNamespace

  // Establish the expected network prefix for addresses
  implicit val netPrefix: NetworkPrefix = networkPrefix

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    //TODO: enable gjallarhorn to create raw transaction.
    case (method, params, id) if method == s"${namespace.name}_createRawTransaction" =>
      createRawTransaction(params.head, id)

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
    * //@param params input parameter as specified above
    * //@param id request identifier
    * @return
    */
  private def createRawTransaction (params: Json, id: String): Future[Json] = {
    val p = params.hcursor
    (for {
      txType <- p.get[String]("txType")
      online <- p.get[Boolean]("online")
      sender <- p.get[IndexedSeq[Address]]("sender")
    } yield {
      val futureTx = txType match {
        case "PolyTransfer" => rawPolyTransfer(p)
        case "ArbitTransfer" => rawArbitTransfer(p)
        case "AssetTransfer" => rawAssetTransfer(p)
        case _ => throw new Exception(s"Transaction type $txType is not valid")
      }

      if (online) {
        val tx = Await.result(futureTx.mapTo[Json], 10.seconds)
        val rawTx = (tx \\ "rawTx").head
        val msgToSign = (tx \\ "messageToSign").head.asString.get
        val signedTx = Await.result((keyManagerRef ? SignTx(rawTx, sender, msgToSign))
          .mapTo[Json], 10.seconds)
        Future{(requests.broadcastTx(signedTx) \\ "result").head}
      }else{
        futureTx
      }
    }) match {
      case Right(value) => value
      case Left(error) => throw new Exception(s"error parsing raw tx: $error")
    }
  }

  def rawPolyTransfer(p: HCursor): Future[Json] = {
    (for {
      propType          <- p.get[String]("propositionType")
      recipients        <- p.get[IndexedSeq[(Address, Long)]]("recipients")
      sender            <- p.get[IndexedSeq[Address]]("sender")
      changeAddr        <- p.get[Address]("changeAddress")
      fee               <- p.get[Long]("fee")
      data              <- p.get[Option[String]]("data")
    } yield {

      val toReceive = recipients.map( r => r._1 -> SimpleValue(r._2))

      (walletManagerRef ? GetWallet).mapTo[MMap[Address,MMap[BoxId, Box]]].map { walletBoxes =>
        propType match {
          case PublicKeyPropositionCurve25519.`typeString` =>
            TransferTransaction
              .createRawTransferParams(walletBoxes, toReceive, sender, changeAddr, None, fee, "PolyTransfer")
              .map { case (inputs, outputs) =>
                TransferTransaction[PublicKeyPropositionCurve25519](inputs, outputs, Map(), fee,
                  Instant.now.toEpochMilli, data, minting = false, "PolyTransfer")
              } match {
              case Success(tx) =>
                Map(
                  "rawTx" -> tx.asJson,
                  "messageToSign" -> Base58.encode(tx.messageToSign).asJson
                ).asJson
              case Failure(ex) => throw new Exception(s"Failed to create raw transaction with error: $ex")
            }

          case ThresholdPropositionCurve25519.`typeString` =>
            TransferTransaction
              .createRawTransferParams(walletBoxes, toReceive, sender, changeAddr, None, fee, "PolyTransfer")
              .map { case (inputs, outputs) =>
                TransferTransaction[ThresholdPropositionCurve25519](inputs, outputs, Map(), fee,
                  Instant.now.toEpochMilli, data, minting = false, "PolyTransfer")
              } match {
              case Success(tx) =>
                Map(
                "rawTx" -> tx.asJson,
                "messageToSign" -> Base58.encode(tx.messageToSign).asJson
              ).asJson
              case Failure(ex) => throw new Exception(s"Failed to create raw transaction with error: $ex")
            }
        }
      }

    }) match {
      case Right(value) => value
      case Left(error) => throw new Exception(s"error parsing raw tx: $error")
    }
  }

  private[http] def rawArbitTransfer(p: HCursor): Future[Json] = {
    (for {
      propType          <- p.get[String]("propositionType")
      recipients        <- p.get[IndexedSeq[(Address, Long)]]("recipients")
      sender            <- p.get[IndexedSeq[Address]]("sender")
      changeAddr        <- p.get[Address]("changeAddress")
      consolidationAddr <- p.get[Option[Address]]("consolidationAddress")
      fee               <- p.get[Long]("fee")
      data              <- p.get[Option[String]]("data")
    } yield {

      val toReceive = recipients.map( r => r._1 -> SimpleValue(r._2))

      (walletManagerRef ? GetWallet).mapTo[MMap[Address,MMap[BoxId, Box]]].map { walletBoxes =>
        propType match {
          case PublicKeyPropositionCurve25519.`typeString` =>
            TransferTransaction
              .createRawTransferParams(walletBoxes, toReceive, sender, changeAddr, consolidationAddr, fee, "ArbitTransfer")
              .map { case (inputs, outputs) =>
                TransferTransaction[PublicKeyPropositionCurve25519](inputs, outputs, Map(), fee,
                  Instant.now.toEpochMilli, data, minting = false, "ArbitTransfer")
              } match {
              case Success(tx) =>
                Map(
                  "rawTx" -> tx.asJson,
                  "messageToSign" -> Base58.encode(tx.messageToSign).asJson
                ).asJson
              case Failure(ex) => throw new Exception(s"Failed to create raw transaction with error: $ex")
            }

          case ThresholdPropositionCurve25519.`typeString` =>
            TransferTransaction
              .createRawTransferParams(walletBoxes, toReceive, sender, changeAddr, consolidationAddr, fee, "ArbitTransfer")
              .map { case (inputs, outputs) =>
                TransferTransaction[ThresholdPropositionCurve25519](inputs, outputs, Map(), fee,
                  Instant.now.toEpochMilli, data, minting = false, "ArbitTransfer")
              } match {
              case Success(tx) =>
                Map(
                  "rawTx" -> tx.asJson,
                  "messageToSign" -> Base58.encode(tx.messageToSign).asJson
                ).asJson
              case Failure(ex) => throw new Exception(s"Failed to create raw transaction with error: $ex")
            }
        }
      }

    }) match {
      case Right(value) => value
      case Left(error) => throw new Exception(s"error parsing raw tx: $error")
    }
  }

  private[http] def rawAssetTransfer(p: HCursor): Future[Json] = {
    (for {
      propType          <- p.get[String]("propositionType")
      recipients        <- p.get[IndexedSeq[(Address, Long)]]("recipients")
      issuer            <- p.get[Address] ("issuer")
      shortName         <- p.get[String]("shortName")
      sender            <- p.get[IndexedSeq[Address]]("sender")
      changeAddr        <- p.get[Address]("changeAddress")
      consolidationAddr <- p.get[Option[Address]]("consolidationAddress")
      minting           <- p.get[Boolean]("minting")
      fee               <- p.get[Long]("fee")
      data              <- p.get[Option[String]]("data")
    } yield {

      val assetCode = Try(AssetCode(1.toByte, issuer, shortName)) match {
        case Success(code) => code
        case Failure(ex) => throw new Exception (s"Unable to generate asset code: $ex")
      }

      val toReceive = recipients.map( r => r._1 -> AssetValue(r._2, assetCode))

      (walletManagerRef ? GetWallet).mapTo[MMap[Address,MMap[BoxId, Box]]].map { walletBoxes =>
        propType match {
          case PublicKeyPropositionCurve25519.`typeString` =>
            TransferTransaction
              .createRawTransferParams(walletBoxes, toReceive, sender, changeAddr, consolidationAddr, fee, "AssetTransfer")
              .map { case (inputs, outputs) =>
                TransferTransaction[PublicKeyPropositionCurve25519](inputs, outputs, Map(), fee,
                  Instant.now.toEpochMilli, data, minting, "AssetTransfer")
              } match {
              case Success(tx) =>
                Map(
                  "rawTx" -> tx.asJson,
                  "messageToSign" -> Base58.encode(tx.messageToSign).asJson
                ).asJson
              case Failure(ex) => throw new Exception(s"Failed to create raw transaction with error: $ex")
            }

          case ThresholdPropositionCurve25519.`typeString` =>
            TransferTransaction
              .createRawTransferParams(walletBoxes, toReceive, sender, changeAddr, consolidationAddr, fee, "AssetTransfer")
              .map { case (inputs, outputs) =>
                TransferTransaction[ThresholdPropositionCurve25519](inputs, outputs, Map(), fee,
                  Instant.now.toEpochMilli, data, minting, "AssetTransfer")
              } match {
              case Success(tx) =>
                Map(
                  "rawTx" -> tx.asJson,
                  "messageToSign" -> Base58.encode(tx.messageToSign).asJson
                ).asJson
              case Failure(ex) => throw new Exception(s"Failed to create raw transaction with error: $ex")
            }
        }
      }

    }) match {
      case Right(value) => value
      case Left(error) => throw new Exception(s"error parsing raw tx: $error")
    }
  }

  /** #### Summary
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
