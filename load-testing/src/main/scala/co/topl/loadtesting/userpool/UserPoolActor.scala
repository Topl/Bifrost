package co.topl.loadtesting.userpool

import cats.implicits._
import akka.actor.ActorSystem
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Source}
import akka.util.Timeout
import akka.{Done, NotUsed}
import co.topl.akkahttprpc._
import co.topl.attestation.Address
import co.topl.loadtesting._
import co.topl.loadtesting.statistics.StatisticsSink
import co.topl.loadtesting.transactions.TransactionTracker
import co.topl.loadtesting.user.UserActor
import co.topl.rpc.ToplRpc.Transaction.BroadcastTx
import co.topl.rpc.ToplRpc.Transaction.BroadcastTx.Response
import co.topl.utils.NetworkType.NetworkPrefix
import com.nike.fleam.implicits._

import scala.concurrent.ExecutionContext
import scala.util.Random

/**
 * Represents a pool of users that trade Polys and Assets among each-other.
 */
object UserPoolActor {

  import co.topl.loadtesting.userpool.implicits._

  /**
   * A collection of data used to identify a user in the pool.
   * @param address the user's address
   * @param actorRef the user's actor reference
   */
  private case class UserData(address: Address, actorRef: ActorRef[UserActor.Command])

  /**
   * The state of the user pool
   * @param users the collection of users in the pool
   * @param keys the actor ref which manages the key ring
   * @param statisticsFile the file to output statistics and results to
   */
  private case class PoolState(users: List[UserData], keys: ActorRef[KeysActor.Command], statisticsFile: String)

  sealed trait Command

  /**
   * Adds new users with the given set of addresses to the user pool.
   * @param addresses the addresses to create users from
   */
  case class AddUsers(addresses: Set[Address]) extends Command

  /**
   * Tracks a transaction containing a Poly transfer to determine if it has been accepted into a block.
   * @param networkPrefix the Bifrost network prefix
   * @param actorSystem the classic actor system for the parent actor
   * @param requestModifier the HTTP request modifier to apply to RPC calls
   * @param executionContext the parent's execution context to run the flow in
   * @return a flow which takes `Either[user.SendPolysFailure, Response]` and outputs `Option[PolyTransferResult]`
   */
  private def trackPolyTx(implicit
    networkPrefix:    NetworkPrefix,
    actorSystem:      ActorSystem,
    requestModifier:  RequestModifier,
    executionContext: ExecutionContext
  ): Flow[Either[user.SendPolysFailure, Response], Option[PolyTransferResult], NotUsed] =
    Flow[Either[user.SendPolysFailure, BroadcastTx.Response]]
      .viaRight(
        Flow[BroadcastTx.Response]
          .mapAsync(1)(r => TransactionTracker(transactionTrackingTimeout, r.id))
          .map {
            case TransactionTracker.TransactionConfirmed(txId, seconds) => PolyTransferSuccess(txId, seconds)
            case TransactionTracker.TransactionUnconfirmed(txId)        => PolyTransferUnconfirmed(txId)
          }
          .map(_.some)
      )
      .viaLeft(
        Flow[user.SendPolysFailure]
          .map {
            case user.NotEnoughPolysForPolyTx(_) => None
            case user.NoContacts                 => None
            case user.PolysRpcFailure(failure)   => PolyTransferFailure(failure).some
          }
      )
      .map {
        case Left(value)  => value
        case Right(value) => value
      }

  /**
   * Tracks a transaction containing an Asset transfer to determine if it has been accepted into a block.
   * @param networkPrefix the Bifrost network prefix
   * @param actorSystem the classic actor system for the parent actor
   * @param requestModifier the HTTP request modifier to apply to RPC calls
   * @param executionContext the parent's execution context to run the flow in
   * @return a flow which takes `Either[user.SendAssetsFailure, Response]` and outputs `Option[AssetTransferResult]`
   */
  private def trackAssetTx(implicit
    networkPrefix:    NetworkPrefix,
    actorSystem:      ActorSystem,
    requestModifier:  RequestModifier,
    executionContext: ExecutionContext
  ): Flow[Either[user.SendAssetsFailure, Response], Option[AssetTransferResult], NotUsed] =
    Flow[Either[user.SendAssetsFailure, BroadcastTx.Response]]
      .viaRight(
        Flow[BroadcastTx.Response]
          .mapAsync(1)(r => TransactionTracker(transactionTrackingTimeout, r.id))
          .map {
            case TransactionTracker.TransactionConfirmed(txId, seconds) => AssetTransferSuccess(txId, seconds)
            case TransactionTracker.TransactionUnconfirmed(txId)        => AssetTransferUnconfirmed(txId)
          }
          .map(_.some)
      )
      .viaLeft(
        Flow[user.SendAssetsFailure]
          .map {
            case user.NotEnoughPolysForAssetTx(_) => None
            case user.AssetsRpcFailure(failure) => AssetTransferFailure(failure).some
          }
      )
      .map {
        case Left(value)  => value
        case Right(value) => value
      }

  /**
   * Instantiates an akka actor behavior which loops on user behavior of sending Polys and Assets.
   * @param user the user actor to make requests to
   * @param statsFile the statistics/results output file
   * @param networkPrefix the Bifrost network prefix
   * @param requestModifier the HTTP request modifier to apply to RPC calls
   * @param timeout the time to wait for responses from actors
   * @return
   */
  private def userLoop(user: UserData, statsFile: String)(implicit
    networkPrefix:           NetworkPrefix,
    requestModifier:         RequestModifier,
    timeout:                 Timeout
  ): Behavior[Done] =
    LoopActor[Done](
      { (_, loopContext) =>
        implicit val loopMaterializer: Materializer = Materializer(loopContext)
        implicit val loopActorSystem: ActorSystem = loopContext.system.classicSystem
        implicit val loopEc: ExecutionContext = loopContext.executionContext

        val random = new Random()
        val sendPolys = random.nextBoolean()

        Source
          .single(NotUsed)
          .via(
            if (sendPolys)
              tryPolyTxFlow(user.actorRef)
                .via(trackPolyTx)
                .via(filterByDefinedFlow)
                .wireTap(x => loopContext.log.debug(x.toString))
                .wireTap(logToConsoleSink[PolyTransferResult])
                .wireTap(StatisticsSink[PolyTransferResult](statsFile))
                .map(_ => NotUsed)
            else
              tryAssetTxFlow(user.actorRef)
                .via(trackAssetTx)
                .via(filterByDefinedFlow)
                .wireTap(x => loopContext.log.debug(x.toString))
                .wireTap(logToConsoleSink[AssetTransferResult])
                .wireTap(StatisticsSink[AssetTransferResult](statsFile))
                .map(_ => NotUsed)
          )
          .run()
      },
      _ => Done
    )

  /**
   * Adds users to the user pool.
   * @param users the addresses to generate users from and add to the pool
   * @param state the current state of the user pool
   * @param context the user pool actor context
   * @param networkPrefix the Bifrost network prefix
   * @param timeout the time to wait for responses from actors
   * @param requestModifier the HTTP request modifier to apply to RPC calls
   * @return a user pool akka behavior
   */
  private def addUsers(users: Set[Address], state: PoolState, context: ActorContext[Command])(implicit
    networkPrefix:            NetworkPrefix,
    timeout:                  Timeout,
    requestModifier:          RequestModifier
  ) = {

    users.foreach(u => context.log.info(u.toString))

    val newUsers =
      users.map(addr =>
        UserData(addr, context.spawn(user.UserActor(addr, state.keys, state.statisticsFile), addr.toString))
      )

    // add new users to existing users contacts
    state.users.foreach(_.actorRef ! UserActor.AddContacts(newUsers.map(_.address).toList))

    // add new users to all new users contacts (including selves)
    newUsers.foreach(_.actorRef ! UserActor.AddContacts(newUsers.map(_.address).toList))

    // add existing users to all new users contacts
    newUsers.foreach(_.actorRef ! UserActor.AddContacts(state.users.map(_.address)))

    implicit val parentContext: ActorContext[Command] = context

    // start new users
    newUsers.foreach { user =>
      val loopActor = context.spawn(userLoop(user, state.statisticsFile), s"UserLoop_${user.address.toString}")
      loopActor ! Done
    }

    withState(state.copy(users = state.users ++ newUsers))
  }

  /**
   * Instantiates a user pool actor behavior from the given `PoolState`.
   * @param state the state of the user pool
   * @param networkPrefix the Bifrost network prefix
   * @param timeout the amount of time to wait for actor responses
   * @param requestModifier the HTTP request modifier to apply to RPC calls
   * @return the user pool behavior for the given state
   */
  private def withState(
    state: PoolState
  )(implicit
    networkPrefix:   NetworkPrefix,
    timeout:         Timeout,
    requestModifier: RequestModifier
  ): Behavior[Command] =
    Behaviors.receive { (context, message) =>
      implicit val materializer: Materializer = Materializer(context)
      implicit val actorSystem: ActorSystem = context.system.classicSystem
      implicit val ec: ExecutionContext = context.executionContext

      message match {
        case AddUsers(addresses) => addUsers(addresses, state, context)
      }
    }

  /**
   * Instantiates a new user pool actor.
   * @param keys an actor which contains the key ring to use for signing
   * @param statsFile the output statistics file
   * @param networkPrefix the Bifrost network prefix
   * @param timeout the amount of time to wait for actor responses
   * @param requestModifier the HTTP request modifier to apply to RPC calls
   * @return a user pool actor behavior
   */
  def apply(
    keys:                   ActorRef[KeysActor.Command],
    statsFile:              String
  )(implicit networkPrefix: NetworkPrefix, timeout: Timeout, requestModifier: RequestModifier): Behavior[Command] =
    withState(PoolState(List(), keys, statsFile))
}
