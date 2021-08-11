package co.topl.loadtesting

import akka.actor.ActorSystem
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Sink, Source}
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

  import PolyTransferResult._
  import AssetTransferResult._

  sealed trait Command
  case class AddUsers(addresses: Set[Address]) extends Command
  case class UserPolyLoop(user: PolyUserActor.UserData) extends Command

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
  ): Flow[Either[PolyUserActor.SendPolysFailure, Response], Option[PolyTransferResult], NotUsed] =
    Flow[Either[PolyUserActor.SendPolysFailure, BroadcastTx.Response]]
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
        Flow[PolyUserActor.SendPolysFailure]
          .map {
            case PolyUserActor.NotEnoughPolys(_)   => None
            case PolyUserActor.NoContacts          => None
            case PolyUserActor.RpcFailure(failure) => PolyTransferFailure(failure).some
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
  ): Flow[Either[AssetUserActor.SendAssetsFailure, Response], Option[AssetTransferResult], NotUsed] =
    Flow[Either[AssetUserActor.SendAssetsFailure, BroadcastTx.Response]]
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
        Flow[AssetUserActor.SendAssetsFailure]
          .map { case AssetUserActor.RpcFailure(failure) =>
            AssetTransferFailure(failure).some
          }
      )
      .map {
        case Left(value)  => value
        case Right(value) => value
      }

  private def polyUserLoop(user: PolyUserActor.UserData, statsFile: String)(implicit
    networkPrefix:               NetworkPrefix,
    requestModifier:             RequestModifier,
    timeout:                     Timeout,
    parentContext:               ActorContext[_]
  ): Behavior[Done] =
    LoopActor[Done](
      { (_, loopContext) =>
        implicit val loopMaterializer: Materializer = Materializer(loopContext)
        implicit val loopActorSystem: ActorSystem = loopContext.system.classicSystem
        implicit val loopEc: ExecutionContext = loopContext.executionContext

        // wait between 100 and 1000 ms
        Thread.sleep(new Random().between(100, 1000))

        Source
          .single(NotUsed)
          .via(tryPolyTxFlow(user.actorRef))
          .via(trackPolyTx)
          .via(filterByDefinedFlow)
          .wireTap(logToConsoleSink[PolyTransferResult])
          .wireTap(StatisticsSink[PolyTransferResult](statsFile))
          .run()
      },
      _ => Done
    )

  private def assetUserLoop(user: AssetUserActor.UserData, statsFile: String)(implicit
    networkPrefix:                NetworkPrefix,
    requestModifier:              RequestModifier,
    timeout:                      Timeout,
    parentContext:                ActorContext[_]
  ): Behavior[Done] =
    LoopActor[Done](
      { (_, loopContext) =>
        implicit val loopMaterializer: Materializer = Materializer(loopContext)
        implicit val loopActorSystem: ActorSystem = loopContext.system.classicSystem
        implicit val loopEc: ExecutionContext = loopContext.executionContext

        // wait between 100 and 1000 ms
        Thread.sleep(new Random().between(100, 1000))

        Source
          .single(NotUsed)
          .via(tryAssetTxFlow(user.actorRef))
          .via(trackAssetTx)
          .via(filterByDefinedFlow)
          .wireTap(logToConsoleSink[AssetTransferResult])
          .wireTap(StatisticsSink[AssetTransferResult](statsFile))
          .run()
      },
      _ => Done
    )

  def apply(
    keys:                   ActorRef[KeysActor.Command],
    statsFile:              String
  )(implicit networkPrefix: NetworkPrefix, timeout: Timeout, requestModifier: RequestModifier): Behavior[Command] =
    withState(List(), List(), keys, statsFile)

  def withState(
    polyUsers:  List[PolyUserActor.UserData],
    assetUsers: List[AssetUserActor.UserData],
    keys:       ActorRef[KeysActor.Command],
    statsFile:  String
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
        case AddUsers(addresses) =>
          val newPolyUsers =
            addresses.map(addr =>
              PolyUserActor.UserData(
                addr,
                context.spawn(PolyUserActor(addr, keys, statsFile), s"PolyUser_${addr.toString}")
              )
            )

          val newAssetUsers =
            addresses.map(addr =>
              AssetUserActor.UserData(
                addr,
                context.spawn(AssetUserActor(addr, keys, statsFile), s"AssetUser_${addr.toString}")
              )
            )

          // add new users to existing users contacts
          polyUsers.foreach(_.actorRef ! PolyUserActor.AddContacts(newPolyUsers.toList))
          assetUsers.foreach(_.actorRef ! AssetUserActor.AddContacts(newAssetUsers.toList))

          // add new users to all new users contacts (including selves)
          newPolyUsers.foreach(_.actorRef ! PolyUserActor.AddContacts(newPolyUsers.toList))
          newAssetUsers.foreach(_.actorRef ! AssetUserActor.AddContacts(newAssetUsers.toList))

          // add existing users to all new users contacts
          newPolyUsers.foreach(_.actorRef ! PolyUserActor.AddContacts(polyUsers))
          newAssetUsers.foreach(_.actorRef ! AssetUserActor.AddContacts(assetUsers))

          implicit val parentContext: ActorContext[Command] = context

          // start new users
          newPolyUsers.foreach { user =>
            val loopActor = context.spawn(polyUserLoop(user, statsFile), s"PolyUserLoop_${user.addr}")
            loopActor ! Done
          }

          newAssetUsers.foreach { user =>
            val loopActor = context.spawn(assetUserLoop(user, statsFile), s"AssetUserLoop_${user.addr}")
            loopActor ! Done
          }

          withState(polyUsers ++ newPolyUsers, assetUsers ++ newAssetUsers, keys, statsFile)
      }
    }
}
