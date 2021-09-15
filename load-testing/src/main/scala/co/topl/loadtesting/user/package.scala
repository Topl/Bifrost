package co.topl.loadtesting

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Source}
import cats.Show
import cats.implicits._
import co.topl.akkahttprpc._
import co.topl.akkahttprpc.implicits.client._
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.loadtesting.statistics._
import co.topl.modifier.ModifierId
import co.topl.modifier.transaction.Transaction.TX
import co.topl.rpc.ToplRpc.NodeView.{Balances, TransactionById}
import co.topl.rpc.ToplRpc.Transaction.BroadcastTx
import co.topl.rpc.ToplRpc.Transaction.BroadcastTx.Response
import co.topl.rpc.implicits.client._
import co.topl.utils.NetworkType.NetworkPrefix
import com.nike.fleam.implicits._

import java.time.LocalDateTime
import scala.collection.immutable.ListMap
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

package object user {

  /**
   * Represents a failure with retrieving the balance of an address
   * @param failure the balance retrieval failure
   */
  case class BalanceUpdateFailed(failure: RpcClientFailure)

  /**
   * The result of a user action.
   * @tparam F the type of failure
   * @tparam S the type of success
   */
  type UserActionResult[F, S] = Either[Either[BalanceUpdateFailed, F], S]

  private[user] val random = new Random()

  /**
   * Flow to broadcast a transaction to a Bifrost node.
   * @param requestModifier an HTTP request modifier
   * @param networkPrefix the Bifrost network to call
   * @param actorSystem the actor system to send an RPC request from
   * @param executionContext the current asynchronous execution context
   * @return a `Flow` which takes in a `TX` and returns either an `RpcClientFailure` or a `BroadcastTx.Response`
   */
  def broadcastTxFlow(implicit
    requestModifier:  RequestModifier,
    networkPrefix:    NetworkPrefix,
    actorSystem:      ActorSystem,
    executionContext: ExecutionContext
  ): Flow[TX, Either[RpcClientFailure, Response], NotUsed] =
    Flow[TX].map(BroadcastTx.Params).mapAsync(1)(BroadcastTx.rpc(_).value)

  /**
   * Flow to run a user action for a user created from a given address and contacts.
   * @param action the user action to run given the user context
   * @param address the address of the user
   * @param contacts the contacts that the user has knowledge of
   * @param sign a function for signing messages using the user's keys
   * @param userAction the `UserAction` type-class instance for the given action
   * @param actorSystem the actor system to send RPC requests from
   * @param networkPrefix the prefix of the Bifrost network
   * @param requestModifier the modifier for HTTP requests
   * @param executionContext the current asynchronous execution context
   * @tparam A the type of the user action
   * @tparam F the failure type of the user action
   * @tparam S the success type of the user action
   * @return a `Flow` with takes in a `NotUsed` value and returns the result of running the action
   */
  def userFlow[A, F: Show: ToStatisticsCsvLog, S: Show: ToStatisticsCsvLog](
    action:   A,
    address:  Address,
    contacts: List[Address],
    sign:     Array[Byte] => Future[ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519]]
  )(implicit
    userAction:       UserAction[A, F, S],
    actorSystem:      ActorSystem,
    networkPrefix:    NetworkPrefix,
    requestModifier:  RequestModifier,
    executionContext: ExecutionContext
  ): Flow[NotUsed, UserActionResult[F, S], NotUsed] =
    Flow[NotUsed]
      .map(_ => List(address))
      // lookup the balance for the user's address
      .map(Balances.Params)
      .mapAsync(1)(Balances.rpc(_).value)
      .eitherLeftMap(failure => BalanceUpdateFailed(failure).asLeft[F])
      .viaRight(
        Flow[Balances.Response]
          .map(response => response(address))
      )
      // run the provided user action
      .viaRight(userAction.actionFlow(action, address, contacts, sign))
      // flatten the action result into a `UserActionResult`
      .eitherFlatMap {
        case Right(success) => success.asRight
        case Left(failure)  => failure.asRight.asLeft
      }

  sealed trait TransactionResult
  case class TransactionConfirmed(seconds: Int) extends TransactionResult
  case class TransactionUnconfirmed() extends TransactionResult

  /**
   * Polls indefinitely for a transaction to be confirmed into a block.
   * @param txId the ID of the transaction to poll for
   * @param networkPrefix the prefix of the Bifrost network
   * @param actorSystem the actor system to send RPC requests from
   * @param requestModifier a modifier for HTTP requests
   * @param ec the current asynchronous execution context
   * @return the result of the transaction wrapped as a future
   */
  def trackTx(txId:  ModifierId)(implicit
    networkPrefix:   NetworkPrefix,
    actorSystem:     ActorSystem,
    requestModifier: RequestModifier,
    ec:              ExecutionContext
  ): Future[TransactionResult] = {
    /*
    Sends transaction status requests once every second and stops when a transaction is signaled as completed.
    Runs a fold on the requests until a transaction is confirmed and returns the time.
     */
    Source(LazyList.from(0))
      .throttle(1, 1.second)
      .map(count => (count, TransactionById.Params(txId)))
      .mapAsync(1)(x => TransactionById.rpc(x._2).value.map((x._1, _)))
      .takeWhile(_._2.isLeft, inclusive = true)
      .runFold(TransactionUnconfirmed(): TransactionResult) {
        case (x: TransactionConfirmed, _)          => x
        case (_, (time, result)) if result.isRight => TransactionConfirmed(time)
        case _                                     => TransactionUnconfirmed()
      }
  }

  implicit val txBroadcastToCsv: ToStatisticsCsvLog[(ModifierId, LocalDateTime)] = { case (modifierId, timestamp) =>
    s"Transaction Broadcast, $modifierId, $timestamp"
  }

  /**
   * Side-affecting operation that users should call when they broadcast a transaction.
   *
   * @param txId the ID of the broacast transaction
   * @param materializer an Akka stream materializer
   */
  def onTxBroadcast(outputPath: String)(txId: ModifierId, timestamp: LocalDateTime)(implicit
    materializer:               Materializer
  ): Unit =
    // log broadcasted TX to CSV file
    Source.single((txId, timestamp)).to(toCsvSink(outputPath)).run()

  object implicits extends SendAssetsAction.Instances with SendPolysAction.Instances
}
