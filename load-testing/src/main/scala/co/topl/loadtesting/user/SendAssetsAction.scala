package co.topl.loadtesting.user

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Source}
import cats.Show
import cats.data.NonEmptyChain
import cats.implicits._
import co.topl.akkahttprpc._
import co.topl.akkahttprpc.implicits.client._
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.loadtesting.statistics._
import co.topl.modifier.ModifierId
import co.topl.modifier.box.{AssetCode, AssetValue}
import co.topl.rpc.ToplRpc
import co.topl.rpc.ToplRpc.NodeView.Balances
import co.topl.rpc.ToplRpc.Transaction.{BroadcastTx, RawAssetTransfer}
import co.topl.rpc.implicits.client._
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.utils.TimeProvider.Time
import com.nike.fleam.implicits._

import java.time.LocalDateTime
import scala.collection.immutable.ListMap
import scala.concurrent.{ExecutionContext, Future}

/**
 * Action to send some assets to another user and log the result.
 * @param numberOfAssetsToMint the number of assets to mint an asset
 * @param fee the transaction fee
 * @param logSuccess a function to log a success
 * @param logFailure a function to log a failure
 * @param onTxBroadcast a function to call when the transaction has been broadcast
 */
case class SendAssetsAction(
  numberOfAssetsToMint: Int,
  fee:                  Int,
  logSuccess:           SendAssetsAction.Success => Unit,
  logFailure:           SendAssetsAction.Failure => Unit,
  onTxBroadcast:        (ModifierId, LocalDateTime) => Unit
)

object SendAssetsAction {

  case class Success(txId: ModifierId, confirmationTime: Int, timestamp: LocalDateTime)

  sealed trait Failure

  /**
   * Indicates that the user's balance does not have enough Polys to complete an Asset TX.
   * @param numPolys the number of Polys currently in this user's balance
   */
  case class NotEnoughPolys(address: Address, numPolys: Int) extends Failure

  /**
   * Indicates that an RPC request has failed while attempting to send Assets.
   * @param failure the error message
   */
  case class RpcFailure(failure: RpcClientFailure) extends Failure

  /**
   * Indicates that a transaction has not been confirmed into a block.
   * @param txId the ID of the transaction
   */
  case class Unconfirmed(txId: ModifierId) extends Failure

  /**
   * Side-affecting function to call when the `SendAssetsAction` has resulted in a success.
   * @param outputPath the file to log success information to
   * @param success the successful result
   * @param materializer an Akka stream materializer
   */
  def onSuccess(outputPath: String)(success: Success)(implicit materializer: Materializer): Unit = {
    import implicits._

    // log the success result to the console and to a csv output file
    println(success.show)
    Source.single(success).to(toCsvSink(outputPath)).run()
  }

  /**
   * Side-affecting function to call when the `SendAssetsAction` has resulted in a failure.
   * @param outputPath the file to log failure information to
   * @param failure the failure result containing the failure information
   * @param materializer an Akka stream materializer
   */
  def onFailure(outputPath: String)(failure: Failure)(implicit materializer: Materializer): Unit = {
    import implicits._

    failure match {
      case _: SendAssetsAction.NotEnoughPolys =>
      case failure                            =>
        // log the failure result to the console and the csv output file
        println(failure.show)
        Source.single(failure).to(toCsvSink(outputPath)).run()
    }
  }

  /**
   * Instantiates a `SendAssetsAction`.
   * @param numAssetsToMint the number of assets to mint
   * @param fee the poly fee to apply to the asset transaction
   * @param successOutput the file path to send information of a successful result to
   * @param failureOutput the file path to send information of a failure to
   * @param broadcastOutput the file path to send broadcast information to
   * @param materializer an Akka stream materializer
   */
  def apply(numAssetsToMint: Int, fee: Int, successOutput: String, failureOutput: String, broadcastOutput: String)(
    implicit materializer:   Materializer
  ): SendAssetsAction =
    new SendAssetsAction(
      numAssetsToMint,
      fee,
      onSuccess(successOutput),
      onFailure(failureOutput),
      onTxBroadcast(broadcastOutput)
    )

  /**
   * Checks that the balance of the given address has the required number of polys.
   * @param address the address of the user
   * @param requiredNumberOfPolys the required number of polys
   * @param balances the balance entry of the address
   * @return either the balance entry with at least the given number of polys or a `NotEnoughPolys` failure
   */
  def checkPolyBalance(
    address:               Address,
    requiredNumberOfPolys: Int,
    balances:              Balances.Entry
  ): Either[NotEnoughPolys, Balances.Entry] =
    Either.cond(
      balances.Balances.Polys >= requiredNumberOfPolys,
      balances,
      NotEnoughPolys(address, balances.Balances.Polys.toInt)
    )

  /**
   * Generates an asset code from the given address.
   * @param address the address to generate an asset code for
   * @return an asset code which the address can mint
   */
  def assetCodeFrom(address: Address): AssetCode =
    AssetCode(1.toByte, address, Latin1Data.unsafe("coffee"))

  /**
   * Generates a transaction which mints or sends an asset to an address from a list of contacts.
   * @param numberOfAssetsToMint the number of assets to mint if no assets are available
   * @param feeAmount the amount of polys to send as a fee
   * @param balances the current address balances
   * @param address the user address
   * @param contacts the list of contacts that can be sent assets
   * @return a `RawAssetTransfer.Params` value containing the transaction information
   */
  def generateTransaction(
    numberOfAssetsToMint: Int,
    feeAmount:            Int,
    balances:             Balances.Entry,
    address:              Address,
    contacts:             List[Address]
  ): RawAssetTransfer.Params =
    ToplRpc.Transaction.RawAssetTransfer.Params(
      propositionType = PublicKeyPropositionCurve25519.typeString,
      sender = NonEmptyChain(address),
      recipients = NonEmptyChain(
        random.shuffle(contacts).headOption.getOrElse(address) ->
        random
          .shuffle(balances.Boxes.AssetBox.map(_.value))
          .headOption
          .getOrElse(AssetValue(numberOfAssetsToMint, assetCodeFrom(address)))
      ),
      fee = feeAmount,
      changeAddress = address,
      consolidationAddress = address,
      minting = balances.Boxes.AssetBox.isEmpty,
      data = Some(Latin1Data.unsafe(random.alphanumeric.take(random.between(0, 127)).mkString))
    )

  /**
   * Instantiates a flow which executes a `SendAssetsAction`.
   * @param action the action to execute
   * @param address the user's address
   * @param contacts the list of contact's that can receive assets
   * @param sign a function for signing a message on behalf of the given address
   * @param actorSystem the actor system to send RPC requests from
   * @param networkPrefix the prefix of the Bifrost network
   * @param executionContext the current asynchronous execution context
   * @param requestModifier a modifier to apply to HTTP requests
   * @return a flow which takes a balance as input and returns an action result
   */
  def executeActionFlow(
    action:   SendAssetsAction,
    address:  Address,
    contacts: List[Address],
    sign:     Array[NetworkPrefix] => Future[ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519]]
  )(implicit
    actorSystem:      ActorSystem,
    networkPrefix:    NetworkPrefix,
    executionContext: ExecutionContext,
    requestModifier:  RequestModifier
  ): Flow[Balances.Entry, Either[Failure, Success], NotUsed] =
    Flow[Balances.Entry]
      // check that the user's poly balance is greater than or equal to the fee
      .map(checkPolyBalance(address, action.fee, _).leftMap(x => x: Failure))
      // create a raw asset transaction
      .eitherMap(balances => generateTransaction(action.numberOfAssetsToMint, action.fee, balances, address, contacts))
      .eitherFlatMapAsync(1)(params => RawAssetTransfer.rpc(params).leftMap(RpcFailure).value)
      .eitherMap(_.rawTx)
      // sign the raw transaction
      .eitherMapAsync(1)(tx => sign(tx.messageToSign).map(signature => tx.copy(attestation = signature)))
      // broadcast the asset transaction if it was created successfully
      .viaRight(
        broadcastTxFlow
          .viaRight(Flow[BroadcastTx.Response].wireTap(x => action.onTxBroadcast(x.id, LocalDateTime.now())))
          .eitherLeftMap(RpcFailure(_): Failure)
      )
      // flatten the broadcast result into an action result
      .eitherFlatMap(x => x)
      // track the transaction until it is added to a block
      .eitherFlatMapAsync(1)(tx =>
        trackTx(tx.id).map {
          case TransactionConfirmed(seconds) => Success(tx.id, seconds, LocalDateTime.now()).asRight
          case TransactionUnconfirmed()      => Unconfirmed(tx.id).asLeft
        }
      )
      // log the result to the console and csv outputs
      .viaRight(Flow[Success].wireTap(action.logSuccess))
      .viaLeft(Flow[Failure].wireTap(action.logFailure))

  trait Instances {

    implicit def userAction(implicit
      actorSystem:      ActorSystem,
      networkPrefix:    NetworkPrefix,
      executionContext: ExecutionContext,
      requestModifier:  RequestModifier
    ): UserAction[SendAssetsAction, Failure, Success] = executeActionFlow

    implicit val sendAssetsSuccessToCsv: ToStatisticsCsvLog[Success] =
      success => s"Asset Transfer, ${success.txId}, ${success.confirmationTime}, ${success.timestamp}"

    implicit val sendAssetsFailureToCsv: ToStatisticsCsvLog[Failure] = {
      case Unconfirmed(txId)           => s"Asset Transfer Unconfirmed, $txId"
      case NotEnoughPolys(addr, polys) => s"Asset Transfer Not Started, Not Enough Polys, $addr, $polys"
      case RpcFailure(RpcErrorFailure(rpcError)) =>
        s"Asset Transfer Failure, Rpc Error, ${rpcError.message}"
      case RpcFailure(HttpExceptionFailure(throwable)) =>
        s"Asset Transfer Failure, HTTP Exception, ${throwable.getMessage}"
      case RpcFailure(UnexpectedResponseFailure(response)) =>
        s"Asset Transfer Failure, Unexpected Response, $response "
    }

    implicit val sendAssetsSuccessShow: Show[Success] =
      success =>
        s"${Console.GREEN}Asset Transfer: " +
        s"TX ID - ${success.txId}, Confirmation Time - ${success.confirmationTime}, " +
        s"Timestamp - ${success.timestamp}${Console.RESET}"

    implicit val sendAssetsFailureShow: Show[Failure] = {
      case Unconfirmed(txId) =>
        s"${Console.YELLOW}Asset Transfer Unconfirmed: TX ID - $txId${Console.RESET}"
      case NotEnoughPolys(address, numPolys) =>
        s"Not enough polys in addr $address to send asset TX: has $numPolys polys"
      case RpcFailure(failure) => s"${Console.RED}Asset Transfer Failed: $failure${Console.RESET}"
    }
  }

  object implicits extends Instances
}
