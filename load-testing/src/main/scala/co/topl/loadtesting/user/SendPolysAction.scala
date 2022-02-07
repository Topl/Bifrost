package co.topl.loadtesting.user

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Source}
import cats.Show
import cats.data.NonEmptyChain
import cats.implicits._
import co.topl.akkahttprpc.implicits.client._
import co.topl.akkahttprpc._
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.loadtesting.statistics._
import co.topl.loadtesting.user.SendAssetsAction.{Failure, Success}
import co.topl.modifier.ModifierId
import co.topl.modifier.transaction.builder.BoxSelectionAlgorithms
import co.topl.rpc.ToplRpc
import co.topl.rpc.ToplRpc.NodeView.Balances
import co.topl.rpc.ToplRpc.Transaction.{BroadcastTx, RawPolyTransfer}
import co.topl.rpc.implicits.client._
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Latin1Data
import com.nike.fleam.implicits._

import java.time.LocalDateTime
import scala.collection.immutable.ListMap
import scala.concurrent.{ExecutionContext, Future}

/**
 * Action to send some polys to another user and log the result.
 * @param amountToSend the number of polys to send to another user
 * @param fee the transaction fee
 * @param logSuccess a function to log a success
 * @param logFailure a function to log a failure
 * @param onTxBroadcast a function to call when the transaction has been broadcast
 */
case class SendPolysAction(
  amountToSend:  Int,
  fee:           Int,
  logSuccess:    SendPolysAction.Success => Unit,
  logFailure:    SendPolysAction.Failure => Unit,
  onTxBroadcast: (ModifierId, LocalDateTime) => Unit
)

object SendPolysAction {

  case class Success(txId: ModifierId, confirmationTime: Int, timestamp: LocalDateTime)

  sealed trait Failure

  /**
   * Indicates that the user's balance does not have enough Polys to complete a Poly TX.
   * @param numPolys the number of Polys currently in this user's balance
   */
  case class NotEnoughPolys(address: Address, numPolys: Int) extends Failure

  /**
   * Indicates that an RPC request has failed while attempting to send Polys.
   * @param failure the error message
   */
  case class RpcFailure(failure: RpcClientFailure) extends Failure

  case class Unconfirmed(txId: ModifierId) extends Failure

  /**
   * Side-affecting function to call when the `SendPolysAction` has resulted in a success.
   * @param outputPath the file to log success information to
   * @param success the successful result
   * @param materializer an Akka stream materializer
   */
  def onSuccess(outputPath: String)(success: SendPolysAction.Success)(implicit materializer: Materializer): Unit = {
    import implicits._

    // log the success result to the console and to a csv output file
    println(success.show)
    Source.single(success).to(toCsvSink(outputPath)).run()
  }

  /**
   * Side-affecting function to call when the `SendPolysAction` has resulted in a failure.
   * @param outputPath the file to log failure information to
   * @param failure the failure result containing the failure information
   * @param materializer an Akka stream materializer
   */
  def onFailure(outputPath: String)(failure: SendPolysAction.Failure)(implicit materializer: Materializer): Unit = {
    import implicits._

    failure match {
      case _: SendPolysAction.NotEnoughPolys =>
      case failure                           =>
        // log the failure result to the console and the csv output file
        println(failure.show)
        Source.single(failure).to(toCsvSink(outputPath)).run()
    }
  }

  /**
   * Instantiates a `SendPolysAction`.
   * @param polysToSend the number of polys to send
   * @param fee the fee to apply to transactions
   * @param successOutput the file path to send information of a successful result to
   * @param failureOutput the file path to send information of a failure to
   * @param broadcastOutput the file path to send broadcast information to
   * @param materializer an Akka streams materializer
   * @return a `SendPolysAction` value
   */
  def apply(
    polysToSend:           Int,
    fee:                   Int,
    successOutput:         String,
    failureOutput:         String,
    broadcastOutput:       String
  )(implicit materializer: Materializer): SendPolysAction =
    new SendPolysAction(
      polysToSend,
      fee,
      onSuccess(successOutput),
      onFailure(failureOutput),
      onTxBroadcast(broadcastOutput)
    )

  /**
   * Checks that the user's balance has the required number of polys
   * @param address the address of the user
   * @param requiredNumberOfPolys the required number of polys to check for
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
   * Generates a transaction which sends polys to an address from a list of contacts.
   * @param sendAmount the amount of polys to send
   * @param feeAmount the amount of polys to send as a fee
   * @param address the user address
   * @param contacts the list of contacts that can be sent assets
   * @return a `RawPolyTransfer.Params` value containing the transaction information
   */
  def generateTransaction(
    sendAmount: Int,
    feeAmount:  Int,
    address:    Address,
    contacts:   List[Address]
  ): RawPolyTransfer.Params =
    ToplRpc.Transaction.RawPolyTransfer.Params(
      propositionType = PublicKeyPropositionCurve25519.typeString,
      sender = NonEmptyChain(address),
      recipients = NonEmptyChain(random.shuffle(contacts).headOption.getOrElse(address) -> sendAmount),
      fee = feeAmount,
      changeAddress = address,
      data = Some(Latin1Data.unsafe(random.alphanumeric.take(random.between(0, 127)).mkString)),
      boxSelectionAlgorithm = BoxSelectionAlgorithms.All
    )

  /**
   * Instantiates a flow which executes a `SendPolysAction`.
   * @param action the action to execute
   * @param address the user's address
   * @param contacts the list of contact's that can receive polys
   * @param sign a function for signing a message on behalf of the given address
   * @param executionContext the current asynchronous execution context
   * @param actorSystem the actor system to send RPC requests from
   * @param networkPrefix the prefix of the Bifrost network
   * @param requestModifier a modifier to apply to HTTP requests
   * @return a flow which takes a balance as input and returns an action result
   */
  def executeActionFlow(
    action:   SendPolysAction,
    address:  Address,
    contacts: List[Address],
    sign:     Array[Byte] => Future[ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519]]
  )(implicit
    executionContext: ExecutionContext,
    actorSystem:      ActorSystem,
    networkPrefix:    NetworkPrefix,
    requestModifier:  RequestModifier
  ): Flow[Balances.Entry, Either[Failure, Success], NotUsed] =
    Flow[Balances.Entry]
      // check that the user's poly balance is greater than or equal to the fee plus the amount of polys to send
      .map(checkPolyBalance(address, action.fee + action.amountToSend, _).leftMap(x => x: Failure))
      // create a raw poly transaction
      .eitherMap(_ => generateTransaction(action.amountToSend, action.fee, address, contacts))
      .eitherFlatMapAsync(1)(params => RawPolyTransfer.rpc(params).leftMap(RpcFailure).value)
      .eitherMap(_.rawTx)
      // sign the raw transaction
      .eitherMapAsync(1)(tx => sign(tx.messageToSign).map(signature => tx.copy(attestation = signature)))
      // broadcast the poly transaction if it was created successfully
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
      requestModifier:  RequestModifier,
      executionContext: ExecutionContext
    ): UserAction[SendPolysAction, Failure, Success] = executeActionFlow

    implicit val sendPolysSuccessToCsv: ToStatisticsCsvLog[Success] =
      success => s"Poly Transfer, ${success.txId}, ${success.confirmationTime}, ${success.timestamp}"

    implicit val sendPolysFailureToCsv: ToStatisticsCsvLog[Failure] = {
      case Unconfirmed(txId)           => s"Poly Transfer Unconfirmed, $txId"
      case NotEnoughPolys(addr, polys) => s"Poly Transfer Not Started, Not Enough Polys, $addr, $polys"
      case RpcFailure(RpcErrorFailure(rpcError)) =>
        s"Poly Transfer Failure, Rpc Error, ${rpcError.message}"
      case RpcFailure(HttpExceptionFailure(throwable)) =>
        s"Poly Transfer Failure, HTTP Exception, ${throwable.getMessage}"
      case RpcFailure(UnexpectedResponseFailure(response)) =>
        s"Poly Transfer Failure, Unexpected Response, $response "
    }

    implicit val sendPolysSuccessShow: Show[Success] =
      success =>
        s"${Console.GREEN}Poly Transfer: " +
        s"TX ID - ${success.txId}, Confirmation Time - ${success.confirmationTime}, " +
        s"Timestamp - ${success.timestamp} ${Console.RESET}"

    implicit val sendPolysFailureShow: Show[Failure] = {
      case Unconfirmed(txId) =>
        s"${Console.YELLOW}Poly Transfer Unconfirmed: TX ID - $txId${Console.RESET}"
      case NotEnoughPolys(address, numPolys) =>
        s"Not enough polys in addr $address to send poly TX: has $numPolys polys"
      case RpcFailure(failure) => s"${Console.RED}Asset Transfer Failed: $failure${Console.RESET}"
    }
  }

  object implicits extends Instances
}
