package co.topl.loadtesting

import akka.NotUsed
import akka.actor.ActorSystem
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Source}
import akka.stream.typed.scaladsl.ActorFlow
import akka.util.Timeout
import co.topl.akkahttprpc.{RequestModifier, RpcClientFailure}
import co.topl.attestation.Address
import co.topl.loadtesting.transactions.TransactionActor
import co.topl.modifier.box.AssetCode
import co.topl.rpc.ToplRpc.Transaction.BroadcastTx
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Latin1Data
import com.nike.fleam.implicits._

import scala.concurrent.ExecutionContext
import scala.util.Random

object AssetUserActor {

  case class UserData(addr: Address, actorRef: ActorRef[AssetUserActor.Command])

  sealed trait Command

  case class AddContacts(users: List[UserData]) extends Command

  case class TrySendAssets(replyTo: ActorRef[Either[SendAssetsFailure, BroadcastTx.Response]]) extends Command

  case class UpdateBalance(replyTo: ActorRef[NotUsed]) extends Command

  private case class BalanceUpdated(assets: Map[AssetCode, Int], replyTo: ActorRef[NotUsed]) extends Command

  sealed trait SendAssetsFailure
  case class RpcFailure(failure: RpcClientFailure) extends SendAssetsFailure

  def withState(
    assets:           Map[AssetCode, Int],
    addr:             Address,
    contacts:         List[UserData],
    transactionActor: ActorRef[TransactionActor.Command]
  )(implicit timeout: Timeout): Behavior[Command] =
    Behaviors.receive { (context, message) =>
      implicit val materializer: Materializer = Materializer(context)

      message match {
        case AddContacts(users) =>
          withState(assets, addr, contacts ++ users, transactionActor)
        case TrySendAssets(replyTo) =>
          val random = new Random()

          val (assetToSend: (AssetCode, Int), mintAsset: Boolean) =
            random
              .shuffle(assets.filter(_._2 > 0))
              .headOption
              .map(_ -> false)
              .getOrElse((AssetCode(1.toByte, addr, Latin1Data.unsafe("coffee")), 1000) -> true)

          val contactToSendTo: Address =
            random
              .shuffle(contacts)
              .headOption
              .map(_.addr)
              .getOrElse(addr)

          Source
            .single(NotUsed)
            .via(
              ActorFlow.ask(transactionActor)((_, rt: ActorRef[Either[RpcClientFailure, BroadcastTx.Response]]) =>
                TransactionActor.SendAssets(addr, contactToSendTo, assetToSend._1, assetToSend._2, mintAsset, rt)
              )
            )
            .viaLeft(Flow[RpcClientFailure].map(RpcFailure))
            .runForeach(replyTo ! _)

          withState(assets, addr, contacts, transactionActor)
        case UpdateBalance(replyTo) =>
          Source
            .single(NotUsed)
            .via(ActorFlow.ask(transactionActor)((_, r) => TransactionActor.GetBalance(r)))
            .viaRight(
              ActorFlow.ask(context.self)((entry, r) =>
                BalanceUpdated(
                  entry(addr).Boxes.AssetBox.map(a => a.value.assetCode -> a.value.quantity.toInt).toMap,
                  r
                )
              )
            )
            .runForeach(_ => replyTo ! NotUsed)

          withState(assets, addr, contacts, transactionActor)
        case BalanceUpdated(newAssetMap, replyTo) =>
          replyTo ! NotUsed
          withState(newAssetMap, addr, contacts, transactionActor)
      }
    }

  def apply(addr:    Address, keys: ActorRef[KeysActor.Command], statsPath: String)(implicit
    networkPrefix:   NetworkPrefix,
    requestModifier: RequestModifier,
    timeout:         Timeout
  ): Behavior[Command] =
    Behaviors.setup { context =>
      val txActor = context.spawn(TransactionActor(keys, addr), s"TransactionActor_AssetUser_$addr")
      implicit val system: ActorSystem = context.system.classicSystem
      implicit val ec: ExecutionContext = context.executionContext

      withState(Map(), addr, List(), txActor)
    }
}
