package co.topl.loadtesting

import akka.actor.ActorSystem
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Source}
import akka.util.Timeout
import akka.{Done, NotUsed}
import cats.Show
import cats.implicits._
import co.topl.akkahttprpc.{RequestModifier, RpcClientFailure}
import co.topl.attestation.Address
import co.topl.loadtesting.transactions.TransactionTracker
import co.topl.modifier.ModifierId
import co.topl.rpc.ToplRpc.Transaction.BroadcastTx
import co.topl.rpc.ToplRpc.Transaction.BroadcastTx.Response
import co.topl.utils.NetworkType.NetworkPrefix
import com.nike.fleam.implicits._

import scala.concurrent.ExecutionContext
import scala.util.Random

object UserPoolActor {

  case class UserData(address: Address, actorRef: ActorRef[UserActor.Command])

  case class PoolState(users: List[UserData], keys: ActorRef[KeysActor.Command], statisticsFile: String)

  sealed trait PolyTransferResult
  case class PolyTransferSuccess(txId: ModifierId, confirmationDelay: Int) extends PolyTransferResult
  case class PolyTransferUnconfirmed(txId: ModifierId) extends PolyTransferResult
  case class PolyTransferFailure(failure: RpcClientFailure) extends PolyTransferResult

  object PolyTransferResult {

    implicit val polyTransferToCsv: ToStatisticsCsvLog[PolyTransferResult] = {
      case PolyTransferSuccess(txId, confirmationDelay) => s"Poly Transfer Confirmed, $txId, $confirmationDelay"
      case PolyTransferUnconfirmed(txId)                => s"Poly Transfer Unconfirmed, $txId"
      case PolyTransferFailure(failure)                 => s"Poly Transfer Failure, $failure"
    }

    implicit val polyTransferShow: Show[PolyTransferResult] = {
      case PolyTransferSuccess(txId, confirmationDelay) =>
        s"${Console.GREEN}Poly Transfer Confirmed: TX ID - $txId, Time - $confirmationDelay${Console.RESET}"
      case PolyTransferUnconfirmed(txId) => s"${Console.YELLOW}Poly Transfer Unconfirmed: TX ID - $txId${Console.RESET}"
      case PolyTransferFailure(failure)  => s"${Console.RED}Poly Transfer Failed: $failure${Console.RESET}"
    }
  }

  sealed trait AssetTransferResult
  case class AssetTransferSuccess(txId: ModifierId, confirmationDelay: Int) extends AssetTransferResult
  case class AssetTransferUnconfirmed(txId: ModifierId) extends AssetTransferResult
  case class AssetTransferFailure(failure: RpcClientFailure) extends AssetTransferResult

  object AssetTransferResult {

    implicit val toCsv: ToStatisticsCsvLog[AssetTransferResult] = {
      case AssetTransferSuccess(txId, confirmationDelay) => s"Asset Transfer Confirmed, $txId, $confirmationDelay"
      case AssetTransferUnconfirmed(txId)                => s"Asset Transfer Unconfirmed, $txId"
      case AssetTransferFailure(failure)                 => s"Asset Transfer Failure, $failure"
    }

    implicit val show: Show[AssetTransferResult] = {
      case AssetTransferSuccess(txId, confirmationDelay) =>
        s"${Console.GREEN}Asset Transfer Confirmed: TX ID - $txId, Time - $confirmationDelay${Console.RESET}"
      case AssetTransferUnconfirmed(txId) =>
        s"${Console.YELLOW}Asset Transfer Unconfirmed: TX ID - $txId${Console.RESET}"
      case AssetTransferFailure(failure) => s"${Console.RED}Asset Transfer Failed: $failure${Console.RESET}"
    }
  }

  import AssetTransferResult._
  import PolyTransferResult._

  sealed trait Command
  case class AddUsers(addresses: Set[Address]) extends Command

  private val transactionTrackingTimeout = 60

  private def filterByDefinedFlow[T]: Flow[Option[T], T, NotUsed] =
    Flow[Option[T]]
      .filter(_.isDefined)
      .map(_.get)

  private def trackPolyTx(implicit
    networkPrefix:    NetworkPrefix,
    actorSystem:      ActorSystem,
    requestModifier:  RequestModifier,
    executionContext: ExecutionContext
  ): Flow[Either[UserActor.SendPolysFailure, Response], Option[PolyTransferResult], NotUsed] =
    Flow[Either[UserActor.SendPolysFailure, BroadcastTx.Response]]
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
        Flow[UserActor.SendPolysFailure]
          .map {
            case UserActor.NotEnoughPolysForPolyTx(_) => None
            case UserActor.NoContacts                 => None
            case UserActor.PolysRpcFailure(failure)   => PolyTransferFailure(failure).some
          }
      )
      .map {
        case Left(value)  => value
        case Right(value) => value
      }

  private def trackAssetTx(implicit
    networkPrefix:    NetworkPrefix,
    actorSystem:      ActorSystem,
    requestModifier:  RequestModifier,
    executionContext: ExecutionContext
  ): Flow[Either[UserActor.SendAssetsFailure, Response], Option[AssetTransferResult], NotUsed] =
    Flow[Either[UserActor.SendAssetsFailure, BroadcastTx.Response]]
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
        Flow[UserActor.SendAssetsFailure]
          .map { case UserActor.AssetsRpcFailure(failure) =>
            AssetTransferFailure(failure).some
          }
      )
      .map {
        case Left(value)  => value
        case Right(value) => value
      }

  private def userLoop(user: UserData, statsFile: String)(implicit
    networkPrefix:           NetworkPrefix,
    requestModifier:         RequestModifier,
    timeout:                 Timeout,
    parentContext:           ActorContext[_]
  ): Behavior[Done] =
    LoopActor[Done](
      { (_, loopContext) =>
        implicit val loopMaterializer: Materializer = Materializer(loopContext)
        implicit val loopActorSystem: ActorSystem = loopContext.system.classicSystem
        implicit val loopEc: ExecutionContext = loopContext.executionContext

        // wait between 100 and 1000 ms
        Thread.sleep(new Random().between(100, 1000))

        val random = new Random()
        val sendPolys = random.nextBoolean()

        Source
          .single(NotUsed)
          .via(
            if (sendPolys)
              tryPolyTxFlow(user.actorRef)
                .via(trackPolyTx)
                .via(filterByDefinedFlow)
                .wireTap(logToConsoleSink[PolyTransferResult])
                .wireTap(StatisticsSink[PolyTransferResult](statsFile))
                .map(_ => NotUsed)
            else
              tryAssetTxFlow(user.actorRef)
                .via(trackAssetTx)
                .via(filterByDefinedFlow)
                .wireTap(logToConsoleSink[AssetTransferResult])
                .wireTap(StatisticsSink[AssetTransferResult](statsFile))
                .map(_ => NotUsed)
          )
          .run()
      },
      _ => Done
    )

  private def addUsers(users: Set[Address], state: PoolState, context: ActorContext[Command])(implicit
    networkPrefix:            NetworkPrefix,
    timeout:                  Timeout,
    requestModifier:          RequestModifier
  ) = {
    val newUsers =
      users.map(addr => UserData(addr, context.spawn(UserActor(addr, state.keys, state.statisticsFile), addr.toString)))

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

  def apply(
    keys:                   ActorRef[KeysActor.Command],
    statsFile:              String
  )(implicit networkPrefix: NetworkPrefix, timeout: Timeout, requestModifier: RequestModifier): Behavior[Command] =
    withState(PoolState(List(), keys, statsFile))

  def withState(
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
}
