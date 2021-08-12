package co.topl.loadtesting

import akka.NotUsed
import akka.actor.ActorSystem
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import akka.stream.scaladsl.{Flow, Source}
import akka.stream.typed.scaladsl.ActorFlow
import akka.util.Timeout
import cats.implicits._
import co.topl.akkahttprpc.{RequestModifier, RpcClientFailure}
import co.topl.attestation.Address
import co.topl.loadtesting.transactions.TransactionActor
import co.topl.modifier.box.AssetCode
import co.topl.rpc.ToplRpc
import co.topl.rpc.ToplRpc.Transaction.BroadcastTx
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Latin1Data
import com.nike.fleam.implicits._

import scala.concurrent.ExecutionContext
import scala.util.Random

/**
 * Maintains a balance of polys and a list of address contacts.
 */
object UserActor {

  case class UserState(
    polyBalance:    Int,
    assetBalance:   Map[AssetCode, Int],
    address:        Address,
    contacts:       List[Address],
    keys:           ActorRef[KeysActor.Command],
    txActor:        ActorRef[TransactionActor.Command],
    statisticsFile: String
  )

  sealed trait Command
  case class AddContacts(users: List[Address]) extends Command
  case class TrySendPolys(replyTo: ActorRef[Either[SendPolysFailure, BroadcastTx.Response]]) extends Command
  case class TrySendAssets(replyTo: ActorRef[Either[SendAssetsFailure, BroadcastTx.Response]]) extends Command
  case class UpdateBalance(replyTo: ActorRef[NotUsed]) extends Command

  private case class BalanceUpdated(polyBalance: Int, assetBalance: Map[AssetCode, Int], replyTo: ActorRef[NotUsed])
      extends Command

  val numberOfPolysToSend = 100
  val numberOfPolysRequired = 1000

  sealed trait SendPolysFailure
  case class NotEnoughPolysForPolyTx(numPolys: Int) extends SendPolysFailure
  case object NoContacts extends SendPolysFailure
  case class PolysRpcFailure(failure: RpcClientFailure) extends SendPolysFailure

  sealed trait SendAssetsFailure
  case class NotEnoughPolysForAssetTx(numPolys: Int) extends SendAssetsFailure
  case class AssetsRpcFailure(failure: RpcClientFailure) extends SendAssetsFailure

  private def addContacts(users: List[Address], state: UserState)(implicit
    networkPrefix:               NetworkPrefix,
    requestModifier:             RequestModifier,
    timeout:                     Timeout,
    actorSystem:                 ActorSystem,
    ec:                          ExecutionContext
  ): Behavior[Command] = withState(state.copy(contacts = state.contacts ++ users))

  private def updateBalance(replyTo: ActorRef[NotUsed], state: UserState, context: ActorContext[Command])(implicit
    networkPrefix:                   NetworkPrefix,
    requestModifier:                 RequestModifier,
    timeout:                         Timeout,
    actorSystem:                     ActorSystem,
    ec:                              ExecutionContext
  ): Behavior[Command] = {
    Source
      .single(NotUsed)
      .via(ActorFlow.ask(state.txActor)((_, r) => TransactionActor.GetBalance(r)))
      .viaRight(Flow[ToplRpc.NodeView.Balances.Response].map(entry => entry(state.address)))
      .viaRight(
        ActorFlow.ask(context.self)((balances, replyTo) =>
          BalanceUpdated(
            balances.Balances.Polys.intValue(),
            balances.Boxes.AssetBox.map(box => box.value.assetCode -> box.value.quantity.toInt).toMap,
            replyTo
          )
        )
      )
      .runForeach(_ => replyTo ! NotUsed)

    Behaviors.same
  }

  private def onBalanceUpdated(polys: Int, assets: Map[AssetCode, Int], replyTo: ActorRef[NotUsed], state: UserState)(
    implicit
    networkPrefix:   NetworkPrefix,
    requestModifier: RequestModifier,
    timeout:         Timeout,
    actorSystem:     ActorSystem,
    ec:              ExecutionContext
  ): Behavior[Command] = {
    replyTo ! NotUsed

    withState(state.copy(polyBalance = polys, assetBalance = assets))
  }

  private def tryToSendPolys(replyTo: ActorRef[Either[SendPolysFailure, BroadcastTx.Response]], state: UserState)(
    implicit
    networkPrefix:   NetworkPrefix,
    requestModifier: RequestModifier,
    timeout:         Timeout,
    actorSystem:     ActorSystem,
    ec:              ExecutionContext
  ): Behavior[Command] = {
    val transactionRequest = for {
      _ <- Either.cond(
        state.polyBalance >= numberOfPolysRequired,
        None,
        NotEnoughPolysForPolyTx(state.polyBalance): SendPolysFailure
      )
      contactToSendTo <- Either.cond(
        state.contacts.nonEmpty,
        state.contacts(new Random().nextInt(state.contacts.length)),
        NoContacts: SendPolysFailure
      )
    } yield TransactionActor.SendPolys(contactToSendTo, numberOfPolysToSend, _)

    Source
      .single(transactionRequest)
      .viaRight(
        ActorFlow.ask(state.txActor)((req, rt: ActorRef[Either[RpcClientFailure, BroadcastTx.Response]]) => req(rt))
      )
      .eitherFlatMap {
        case Left(err: RpcClientFailure) => PolysRpcFailure(err).asLeft
        case Right(response)             => response.asRight
      }
      .runForeach(replyTo ! _)

    Behaviors.same
  }

  private def tryToSendAssets(replyTo: ActorRef[Either[SendAssetsFailure, BroadcastTx.Response]], state: UserState)(
    implicit
    networkPrefix:   NetworkPrefix,
    requestModifier: RequestModifier,
    timeout:         Timeout,
    actorSystem:     ActorSystem,
    ec:              ExecutionContext
  ): Behavior[Command] = {
    val random = new Random()

    val transactionRequest = for {
      _ <- Either.cond(
        state.polyBalance >= numberOfPolysRequired,
        None,
        NotEnoughPolysForAssetTx(state.polyBalance): SendAssetsFailure
      )
      (assetToSend, mintAsset) =
        random
          .shuffle(state.assetBalance.filter(_._2 > 0))
          .headOption
          .map(_ -> false)
          .getOrElse((AssetCode(1.toByte, state.address, Latin1Data.unsafe("coffee")), 1000) -> true)
      contactToSendTo: Address = random.shuffle(state.contacts).headOption.getOrElse(state.address)
    } yield TransactionActor.SendAssets(state.address, contactToSendTo, assetToSend._1, assetToSend._2, mintAsset, _)

    Source
      .single(transactionRequest)
      .viaRight(
        ActorFlow.ask(state.txActor)((req, rt: ActorRef[Either[RpcClientFailure, BroadcastTx.Response]]) => req(rt))
      )
      .eitherFlatMap {
        case Left(err: RpcClientFailure) => AssetsRpcFailure(err).asLeft
        case Right(response)             => response.asRight
      }
      .runForeach(replyTo ! _)

    Behaviors.same
  }

  def apply(addr:    Address, keys: ActorRef[KeysActor.Command], statsPath: String)(implicit
    networkPrefix:   NetworkPrefix,
    timeout:         Timeout,
    requestModifier: RequestModifier
  ): Behavior[Command] =
    Behaviors.setup { context =>
      val txActor = context.spawn(TransactionActor(keys, addr), s"TransactionActor_$addr")
      implicit val system: ActorSystem = context.system.classicSystem
      implicit val ec: ExecutionContext = context.executionContext

      withState(UserState(0, Map(), addr, List(), keys, txActor, statsPath))
    }

  def withState(state: UserState)(implicit
    networkPrefix:     NetworkPrefix,
    requestModifier:   RequestModifier,
    timeout:           Timeout,
    actorSystem:       ActorSystem,
    ec:                ExecutionContext
  ): Behavior[Command] =
    Behaviors.receive { (context, message) =>
      message match {
        case AddContacts(contacts)                  => addContacts(contacts, state)
        case UpdateBalance(replyTo)                 => updateBalance(replyTo, state, context)
        case BalanceUpdated(polys, assets, replyTo) => onBalanceUpdated(polys, assets, replyTo, state)
        case TrySendPolys(replyTo)                  => tryToSendPolys(replyTo, state)
        case TrySendAssets(replyTo)                 => tryToSendAssets(replyTo, state)
      }
    }
}
