package co.topl.loadtesting

import akka.NotUsed
import akka.actor.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.typed.scaladsl.ActorFlow
import akka.util.Timeout
import cats.implicits._
import co.topl.akkahttprpc.{RequestModifier, RpcClientFailure}
import co.topl.attestation.Address
import co.topl.loadtesting.transactions.TransactionActor
import co.topl.rpc.ToplRpc.Transaction.BroadcastTx
import co.topl.utils.NetworkType.NetworkPrefix
import com.nike.fleam.implicits._

import scala.concurrent.ExecutionContext
import scala.util.Random

/**
 * Maintains a balance of polys and a list of address contacts.
 */
object PolyUserActor {
  case class UserData(addr: Address, actorRef: ActorRef[PolyUserActor.Command])

  sealed trait Command

  case class AddContacts(users: List[UserData]) extends Command

  case class TrySendPolys(replyTo: ActorRef[Either[SendPolysFailure, BroadcastTx.Response]]) extends Command

  case class UpdateBalance(replyTo: ActorRef[NotUsed]) extends Command

  private case class BalanceUpdated(polyBalance: Int, replyTo: ActorRef[NotUsed]) extends Command

  val numberOfPolysToSend = 100
  val numberOfPolysRequired = 1000

  sealed trait SendPolysFailure
  case class NotEnoughPolys(numPolys: Int) extends SendPolysFailure
  case object NoContacts extends SendPolysFailure
  case class RpcFailure(failure: RpcClientFailure) extends SendPolysFailure

  def apply(addr:    Address, keys: ActorRef[KeysActor.Command], statsPath: String)(implicit
    networkPrefix:   NetworkPrefix,
    timeout:         Timeout,
    requestModifier: RequestModifier
  ): Behavior[Command] =
    Behaviors.setup { context =>
      val txActor = context.spawn(TransactionActor(keys, addr), s"TransactionActor_PolyUser_$addr")
      implicit val system: ActorSystem = context.system.classicSystem
      implicit val ec: ExecutionContext = context.executionContext

      withState(0, addr, keys, txActor, List(), statsPath)
    }

  def withState(
              balance:          Int,
              addr:             Address,
              keys:             ActorRef[KeysActor.Command],
              transactionActor: ActorRef[TransactionActor.Command],
              contacts:         List[UserData],
              statsPath:        String
  )(implicit
    networkPrefix:   NetworkPrefix,
    requestModifier: RequestModifier,
    timeout:         Timeout,
    actorSystem:     ActorSystem,
    ec:              ExecutionContext
  ): Behavior[Command] =
    Behaviors.receive { (context, message) =>
      message match {

        case AddContacts(users) =>
          context.log.debug(s"${users.length} new contacts added to $addr contacts list")
          withState(balance, addr, keys, transactionActor, contacts ++ users, statsPath)

        case UpdateBalance(replyTo: ActorRef[NotUsed]) =>
          context.log.debug(s"updating balance for address $addr")

          Source
            .single(NotUsed)
            .via(ActorFlow.ask(transactionActor)((_, r) => TransactionActor.GetBalance(r)))
            .wireTap(x => context.log.debug(s"$x"))
            .viaRight(
              ActorFlow.ask(context.self)((entry, replyTo) =>
                BalanceUpdated(entry(addr).Balances.Polys.intValue(), replyTo)
              )
            )
            .wireTap(x => context.log.info(s"$addr balance updated: $x"))
            .to(Sink.foreach(_ => replyTo ! NotUsed))
            .run()

          Behaviors.same

        case BalanceUpdated(balance, replyTo) =>
          context.log.debug(s"$addr balance updated: $balance")

          replyTo ! NotUsed

          withState(balance, addr, keys, transactionActor, contacts, statsPath)

        case TrySendPolys(replyTo) =>
          val transactionRequest = for {
            _ <- Either.cond(
              balance >= numberOfPolysRequired,
              None,
              NotEnoughPolys(balance): SendPolysFailure
            )
            contactToSendTo <- Either.cond(
              contacts.nonEmpty,
              contacts(new Random().nextInt(contacts.length)),
              NoContacts: SendPolysFailure
            )
          } yield TransactionActor.SendPolys(contactToSendTo.addr, numberOfPolysToSend, _)

          Source
            .single(transactionRequest)
            .viaRight(
              ActorFlow.ask(transactionActor)((req, rt: ActorRef[Either[RpcClientFailure, BroadcastTx.Response]]) =>
                req(rt)
              )
            )
            .eitherFlatMap {
              case Left(err: RpcClientFailure) => RpcFailure(err).asLeft
              case Right(response) => response.asRight
            }
            .runForeach(replyTo ! _)

          Behaviors.same
      }
    }
}
