package co.topl.loadtesting.transactions

import akka.actor.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, Scheduler}
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import akka.util.Timeout
import cats.implicits._
import co.topl.akkahttprpc.{RequestModifier, RpcClientFailure}
import co.topl.attestation.Address
import co.topl.loadtesting.KeysActor
import co.topl.modifier.box.AssetCode
import co.topl.rpc.ToplRpc
import co.topl.rpc.ToplRpc.Transaction.BroadcastTx
import co.topl.utils.NetworkType.NetworkPrefix
import com.nike.fleam.implicits._

import scala.concurrent.ExecutionContext

object TransactionActor {
  sealed trait Command

  case class SendPolys(
    to:      Address,
    amount:  Int,
    replyTo: ActorRef[Either[RpcClientFailure, BroadcastTx.Response]]
  ) extends Command

  case class SendAssets(
    from:    Address,
    to:      Address,
    code:    AssetCode,
    amount:  Int,
    minting: Boolean,
    replyTp: ActorRef[Either[RpcClientFailure, BroadcastTx.Response]]
  ) extends Command

  case class GetBalance(replyTo: ActorRef[Either[RpcClientFailure, ToplRpc.NodeView.Balances.Response]]) extends Command

  def apply(keys:    ActorRef[KeysActor.Command], addr: Address)(implicit
    networkPrefix:   NetworkPrefix,
    requestModifier: RequestModifier,
    timeout:         Timeout
  ): Behavior[TransactionActor.Command] =
    Behaviors.setup { context =>
      implicit val scheduler: Scheduler = context.system.scheduler
      implicit val ec: ExecutionContext = context.executionContext
      implicit val actorSystem: ActorSystem = context.system.classicSystem

      withState(keys, addr)
    }

  def withState(keys: ActorRef[KeysActor.Command], address: Address)(implicit
    netPrefix:        NetworkPrefix,
    requestModifier:  RequestModifier,
    system:           ActorSystem,
    scheduler:        Scheduler,
    timeout:          Timeout,
    ec:               ExecutionContext
  ): Behavior[TransactionActor.Command] =
    Behaviors.receive { (context, message) =>
      implicit val materializer: Materializer = Materializer(context)

      message match {

        case SendPolys(to, amount, replyTo) =>
          context.log.debug(s"sending $amount polys from $address to $to")

          Source
            .single(PolyTransferFlow.Req(address, to, amount))
            .via(PolyTransferFlow(keys))
            .viaRight(BroadcastFlow())
            .map {
              case Right(value)  => value
              case Left(failure) => failure.asLeft
            }
            .runForeach(replyTo ! _)

          Behaviors.same

        case SendAssets(from, to, code, amount, minting, replyTo) =>
          Source
            .single(AssetTransferFlow.Req(code, amount, minting, from, to))
            .via(AssetTransferFlow(keys))
            .viaRight(BroadcastFlow())
            .map {
              case Right(value)  => value
              case Left(failure) => failure.asLeft
            }
            .runForeach(replyTo ! _)

          Behaviors.same

        case GetBalance(replyTo) =>
          context.log.debug(s"retrieving balance of $address")

          Source
            .single(address)
            .via(BalanceFlow())
            .wireTap(x => context.log.debug(s"result of balance request: $x"))
            .runForeach(replyTo ! _)

          Behaviors.same
      }
    }
}
