package co.topl.loadtesting

import cats.implicits._
import akka.NotUsed
import akka.actor.ActorSystem
import akka.actor.typed.ActorRef
import akka.stream.scaladsl.{Flow, Sink}
import akka.stream.typed.scaladsl.ActorFlow
import akka.util.Timeout
import cats.Show
import co.topl.akkahttprpc._
import co.topl.loadtesting.statistics.ToStatisticsCsvLog
import co.topl.loadtesting.user.UserActor
import co.topl.modifier.ModifierId
import co.topl.rpc.ToplRpc.Transaction.BroadcastTx.Response
import co.topl.utils.NetworkType.NetworkPrefix

import scala.concurrent.ExecutionContext

package object userpool {

  /**
   * Represents the result of a Poly transfer attempt.
   */
  sealed trait PolyTransferResult

  /**
   * Represents a successful Poly transfer.
   * @param txId the transfer's transaction ID
   * @param confirmationDelay the time it took to confirm the transaction was accepted into a block
   */
  case class PolyTransferSuccess(txId: ModifierId, confirmationDelay: Int) extends PolyTransferResult

  /**
   * Represents a Poly transfer that could not be confirmed in a block.
   * @param txId the ID of the transaction containing the Poly transfer
   */
  case class PolyTransferUnconfirmed(txId: ModifierId) extends PolyTransferResult

  /**
   * Represents a Poly transfer that failed to be created or broadcast.
   * @param failure the RPC failure information
   */
  case class PolyTransferFailure(failure: RpcClientFailure) extends PolyTransferResult

  object PolyTransferResult {

    trait Implicits {

      implicit val polyTransferToCsv: ToStatisticsCsvLog[PolyTransferResult] = {
        case PolyTransferSuccess(txId, confirmationDelay)   => s"Poly Transfer Confirmed, $txId, $confirmationDelay"
        case PolyTransferUnconfirmed(txId)                  => s"Poly Transfer Unconfirmed, $txId"
        case PolyTransferFailure(RpcErrorFailure(rpcError)) => s"Poly Transfer Failure, Rpc Error, ${rpcError.message}"
        case PolyTransferFailure(HttpExceptionFailure(throwable)) =>
          s"Poly Transfer Failure, HTTP Exception, ${throwable.getMessage}"
        case PolyTransferFailure(UnexpectedResponseFailure(response)) =>
          s"Poly Transfer Failure, Unexpected Response, $response "
      }

      implicit val polyTransferShow: Show[PolyTransferResult] = {
        case PolyTransferSuccess(txId, confirmationDelay) =>
          s"${Console.GREEN}Poly Transfer Confirmed: TX ID - $txId, Time - $confirmationDelay${Console.RESET}"
        case PolyTransferUnconfirmed(txId) =>
          s"${Console.YELLOW}Poly Transfer Unconfirmed: TX ID - $txId${Console.RESET}"
        case PolyTransferFailure(failure) => s"${Console.RED}Poly Transfer Failed: $failure${Console.RESET}"
      }
    }
  }

  /**
   * Represents the result of an Asset transfer attempt.
   */
  sealed trait AssetTransferResult

  /**
   * Represents a successful Asset transfer.
   * @param txId the transfer's transaction ID
   * @param confirmationDelay the time it took to confirm the transaction was accepted into a block
   */
  case class AssetTransferSuccess(txId: ModifierId, confirmationDelay: Int) extends AssetTransferResult

  /**
   * Represents an Asset transfer that could not be confirmed in a block.
   * @param txId the ID of the transaction containing the Poly transfer
   */
  case class AssetTransferUnconfirmed(txId: ModifierId) extends AssetTransferResult

  /**
   * Represents an Asset transfer that failed to be created or broadcast.
   * @param failure the RPC failure information
   */
  case class AssetTransferFailure(failure: RpcClientFailure) extends AssetTransferResult

  object AssetTransferResult {

    trait Implicits {

      implicit val toCsv: ToStatisticsCsvLog[AssetTransferResult] = {
        case AssetTransferSuccess(txId, confirmationDelay) => s"Asset Transfer Confirmed, $txId, $confirmationDelay"
        case AssetTransferUnconfirmed(txId)                => s"Asset Transfer Unconfirmed, $txId"
        case AssetTransferFailure(RpcErrorFailure(rpcError)) =>
          s"Asset Transfer Failure, Rpc Error, ${rpcError.message}"
        case AssetTransferFailure(HttpExceptionFailure(throwable)) =>
          s"Asset Transfer Failure, HTTP Exception, ${throwable.getMessage}"
        case AssetTransferFailure(UnexpectedResponseFailure(response)) =>
          s"Asset Transfer Failure, Unexpected Response, $response "
      }

      implicit val show: Show[AssetTransferResult] = {
        case AssetTransferSuccess(txId, confirmationDelay) =>
          s"${Console.GREEN}Asset Transfer Confirmed: TX ID - $txId, Time - $confirmationDelay${Console.RESET}"
        case AssetTransferUnconfirmed(txId) =>
          s"${Console.YELLOW}Asset Transfer Unconfirmed: TX ID - $txId${Console.RESET}"
        case AssetTransferFailure(failure) => s"${Console.RED}Asset Transfer Failed: $failure${Console.RESET}"
      }
    }
  }

  /**
   * The length of time to poll transactions until they are logged as unconfirmed.
   */
  private[userpool] val transactionTrackingTimeout = 60

  /**
   * Filters option values that are defined and unwraps them into their underlying types.
   * @tparam T the type of the underlying value
   * @return the underlying value of an option if it is defined
   */
  private[userpool] def filterByDefinedFlow[T]: Flow[Option[T], T, NotUsed] =
    Flow[Option[T]]
      .filter(_.isDefined)
      .map(_.get)

  /**
   * Attempts a Poly Transaction using the given `UserActor` reference.
   * @param userActor the user actor to request a transaction from
   * @param networkPrefix the current Bifrost network prefix
   * @param requestModifier an HTTP request modifier
   * @param timeout the amount of time to wait for actors to respond
   * @param actorSystem the actor system to make HTTP requests from
   * @param executionContext the execution context to run in
   * @return a flow which returns an `Either[UserActor.SendPolysFailure, Response]` result
   */
  private[userpool] def tryPolyTxFlow(userActor: ActorRef[UserActor.Command])(implicit
    networkPrefix:                               NetworkPrefix,
    requestModifier:                             RequestModifier,
    timeout:                                     Timeout,
    actorSystem:                                 ActorSystem,
    executionContext:                            ExecutionContext
  ): Flow[NotUsed, Either[user.SendPolysFailure, Response], NotUsed] =
    Flow[NotUsed]
      .via(ActorFlow.ask(userActor)((_, r) => UserActor.UpdateBalance(r)))
      .via(ActorFlow.ask(userActor)((_, r) => UserActor.TrySendPolys(r)))

  /**
   * Attempts an Asset Transaction using the given `UserActor` reference.
   * @param userActor the user actor to request a transaction from
   * @param networkPrefix the current Bifrost network prefix
   * @param requestModifier an HTTP request modifier
   * @param timeout the amount of time to wait for actors to respond
   * @param actorSystem the actor system to make HTTP requests from
   * @param executionContext the execution context to run in
   * @return a flow which returns an `Either[UserActor.SendAssetsFailure, Response]` result
   */
  private[userpool] def tryAssetTxFlow(userActor: ActorRef[UserActor.Command])(implicit
    networkPrefix:                                NetworkPrefix,
    requestModifier:                              RequestModifier,
    timeout:                                      Timeout,
    actorSystem:                                  ActorSystem,
    executionContext:                             ExecutionContext
  ): Flow[NotUsed, Either[user.SendAssetsFailure, Response], NotUsed] =
    Flow[NotUsed]
      .via(ActorFlow.ask(userActor)((_, r) => UserActor.UpdateBalance(r)))
      .via(ActorFlow.ask(userActor)((_, r) => UserActor.TrySendAssets(r)))

  def logToConsoleSink[T: Show]: Sink[T, NotUsed] = Flow[T].map(_.show).to(Sink.foreach(println))

  object implicits extends PolyTransferResult.Implicits with AssetTransferResult.Implicits
}
