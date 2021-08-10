package co.topl.loadtesting

import cats.implicits._
import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, scaladsl}
import akka.stream.{Materializer, OverflowStrategy}
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.util.Timeout
import co.topl.akkahttprpc.RequestModifier
import co.topl.attestation.Address
import co.topl.loadtesting.PolyUserActor.{NoContacts, NotEnoughPolys, RpcFailure, SendPolysFailure, UserData}
import co.topl.loadtesting.StatisticsSink.{LogPolyTransferFailure, LogPolyTransferSuccess, LogPolyTransferUnconfirmed}
import co.topl.loadtesting.TrySendPolysFlow.transactionTrackingTimeout
import co.topl.loadtesting.transactions.TransactionTracker
import co.topl.rpc.ToplRpc.Transaction.BroadcastTx
import co.topl.rpc.ToplRpc.Transaction.BroadcastTx.Response
import co.topl.utils.NetworkType.NetworkPrefix
import com.nike.fleam.implicits._

import scala.concurrent.ExecutionContext
import scala.util.Random

object UserPoolActor {
  sealed trait Command
  case class AddUsers(addresses: Set[Address]) extends Command
  case class PolyLoopOnUser(user: UserData) extends Command

  private def filterByDefinedFlow[T]: Flow[Option[T], T, NotUsed] =
    Flow[Option[T]]
      .filter(_.isDefined)
      .map(_.get)

  private def transactionToLogFlow(implicit
    networkPrefix:    NetworkPrefix,
    actorSystem:      ActorSystem,
    requestModifier:  RequestModifier,
    executionContext: ExecutionContext
  ): Flow[Either[SendPolysFailure, Response], Option[StatisticsSink.LogPolyTransfer], NotUsed] =
    Flow[Either[SendPolysFailure, BroadcastTx.Response]]
      .viaRight(
        Flow[BroadcastTx.Response]
          .mapAsync(1)(r => TransactionTracker(transactionTrackingTimeout, r.id).map((r, _)))
          .map(x => if (x._2._1) LogPolyTransferSuccess(x._1, x._2._2) else LogPolyTransferUnconfirmed(x._1))
          .map(_.some)
      )
      .viaLeft(
        Flow[SendPolysFailure]
          .map {
            case NotEnoughPolys(_) => None
            case NoContacts        => None
            case RpcFailure(failure) =>
              LogPolyTransferFailure(s"Poly transfer broadcast failed: $failure").some
          }
      )
      .map {
        case Left(value)  => value
        case Right(value) => value
      }

  private def logPolyTransferToConsoleSink: Sink[StatisticsSink.LogPolyTransfer, NotUsed] =
    Flow[StatisticsSink.LogPolyTransfer]
      .map {
        case LogPolyTransferSuccess(tx, timeDelta) =>
          s"${Console.GREEN}Poly Transfer Success took $timeDelta s${Console.RESET}"
        case LogPolyTransferUnconfirmed(tx) => s"${Console.YELLOW}Poly Transfer #${tx.id} Unconfirmed${Console.RESET}"
        case LogPolyTransferFailure(message) =>
          s"${Console.RED}Poly Transfer failure occurred! $message${Console.RESET}"
      }
      .to(Sink.foreach(println))

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

        Source
          .single(NotUsed)
          .via(TrySendPolysFlow(user.actorRef))
          .via(transactionToLogFlow)
          .via(filterByDefinedFlow)
          .wireTap(logPolyTransferToConsoleSink)
          .wireTap(StatisticsSink(statsFile))
          .run()
      },
      _ => Done
    )

  def apply(
    keys:                   ActorRef[KeysActor.Command],
    statsFile:              String
  )(implicit networkPrefix: NetworkPrefix, timeout: Timeout, requestModifier: RequestModifier): Behavior[Command] =
    withUsers(List(), keys, statsFile)

  def withUsers(users: List[UserData], keys: ActorRef[KeysActor.Command], statsFile: String)(implicit
    networkPrefix:     NetworkPrefix,
    timeout:           Timeout,
    requestModifier:   RequestModifier
  ): Behavior[Command] =
    Behaviors.receive { (context, message) =>
      implicit val materializer: Materializer = Materializer(context)
      implicit val actorSystem: ActorSystem = context.system.classicSystem
      implicit val ec: ExecutionContext = context.executionContext

      message match {
        case AddUsers(addresses) =>
          val newUsers =
            addresses.map(addr => UserData(addr, context.spawn(PolyUserActor(addr, keys, statsFile), addr.toString)))

          // add new users to existing users contacts
          users.foreach(_.actorRef ! PolyUserActor.AddContacts(newUsers.toList))

          // add new users to all new users contacts (including selves)
          newUsers.foreach(_.actorRef ! PolyUserActor.AddContacts(newUsers.toList))

          // add existing users to all new users contacts
          newUsers.foreach(_.actorRef ! PolyUserActor.AddContacts(users))

          implicit val parentContext: ActorContext[Command] = context

          // start new users
          newUsers.foreach { user =>
            val loopActor = context.spawn(userLoop(user, statsFile), s"PolyUserLoop_${user.addr}")
            loopActor ! Done
          }

          withUsers(users ++ newUsers, keys, statsFile)
      }
    }
}
