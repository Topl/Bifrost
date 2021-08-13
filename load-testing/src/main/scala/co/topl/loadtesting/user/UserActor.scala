package co.topl.loadtesting.user

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
import co.topl.loadtesting.KeysActor
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
 * Self-contained user which manages balances, contacts, and sending poly and asset transactions.
 */
object UserActor {

  sealed trait Command

  /**
   * Adds the given list of users to this user's contacts.
   * @param users the users to add
   */
  case class AddContacts(users: List[Address]) extends Command

  /**
   * Attempts to send polys to some user in this user's contacts list.
   * Sends a result message to the given `replyTo` actor.
   * @param replyTo the actor to send the attempt result to
   */
  case class TrySendPolys(replyTo: ActorRef[Either[SendPolysFailure, BroadcastTx.Response]]) extends Command

  /**
   * Attempts to send assets to some user in this user's contacts list.
   * Sends a result message to the given `replyTo` actor.
   * @param replyTo the actor to send the attempt result to
   */
  case class TrySendAssets(replyTo: ActorRef[Either[SendAssetsFailure, BroadcastTx.Response]]) extends Command

  /**
   * Requests this user to update their balance.
   * Sends a completion message to the given `replyTo` actor.
   * @param replyTo the actor to send a completion message to
   */
  case class UpdateBalance(replyTo: ActorRef[NotUsed]) extends Command

  /**
   * Internal message used to update the user's balance state.
   * @param polyBalance the new poly balance
   * @param assetBalance the new asset balance
   * @param replyTo the actor to send a completion message to
   */
  private[user] case class BalanceUpdated(
    polyBalance:  Int,
    assetBalance: Map[AssetCode, Int],
    replyTo:      ActorRef[NotUsed]
  ) extends Command

  /**
   * The current state of the user actor
   * @param polyBalance the current balance of polys held in the address
   * @param assetBalance the current balance of assets held in the address
   * @param address the address this user owns
   * @param contacts the contact addresses this user can send polys and assets to
   * @param keys the keys actor which owns the user's public/private keys
   * @param txActor the user's transaction actor which it can requests transactions from
   */
  private case class UserState(
    polyBalance:  Int,
    assetBalance: Map[AssetCode, Int],
    address:      Address,
    contacts:     List[Address],
    keys:         ActorRef[KeysActor.Command],
    txActor:      ActorRef[TransactionActor.Command]
  )

  /**
   * Adds contacts to a user's contact list.
   * @param users the users to add to this user's contact list
   * @param state the current user state
   * @param networkPrefix the Bifrost network prefix
   * @param requestModifier the HTTP request modifier
   * @param timeout the amount of time to wait for actors to respond
   * @param actorSystem the classic actor system for this typed actor system
   * @param ec the current execution context
   * @return an updated user akka behavior of type `Behavior[Command]`
   */
  private def addContacts(users: List[Address], state: UserState)(implicit
    networkPrefix:               NetworkPrefix,
    requestModifier:             RequestModifier,
    timeout:                     Timeout,
    actorSystem:                 ActorSystem,
    ec:                          ExecutionContext
  ): Behavior[Command] = withState(state.copy(contacts = state.contacts ++ users))

  /**
   * Requests an update of this user's Poly and Asset balances.
   * @param replyTo the actor to reply to on completion of the balance update.
   * @param state the current user state
   * @param context the parent actor context
   * @param networkPrefix the Bifrost network prefix
   * @param requestModifier the HTTP request modifier
   * @param timeout the amount of time to wait for actors to respond
   * @param actorSystem the classic actor system for this typed actor system
   * @param ec the current execution context
   * @return an updated user akka behavior of type `Behavior[Command]`
   */
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

  /**
   * Updates the user's balance on a response from the Bifrost node.
   * @param polys the number of polys the user owns
   * @param assets the number of assets the user owns
   * @param replyTo the actor to reply to with a confirmation message
   * @param state the current user state
   * @param networkPrefix the Bifrost netowrk prefix
   * @param requestModifier the HTTP request modifier
   * @param timeout the amount of time to wait for actors to respond
   * @param actorSystem the classic actor system for this typed actor system
   * @param ec the current execution context
   * @return an updated user akka behavior of type `Behavior[Command]`
   */
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

  /**
   * Requests that the user attempt to send Polys.
   * @param replyTo the actor to send the result to
   * @param state the current user state
   * @param networkPrefix the Bifrost network prefix
   * @param requestModifier the HTTP request modifier
   * @param timeout the amount of time to wait for actors to respond
   * @param actorSystem the classic actor system for this typed actor system
   * @param ec the current execution context
   * @return an updated user akka behavior of type `Behavior[Command]`
   */
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
        state.polyBalance >= numberOfPolysRequiredForPolyOp,
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

  /**
   * Requests that the user attempt to send an owned asset.
   * @param replyTo the actor to send the result to
   * @param state the current user state
   * @param networkPrefix the Bifrost network prefix
   * @param requestModifier the HTTP request modifier
   * @param timeout the amount of time to wait for actors to respond
   * @param actorSystem the classic actor system for this typed actor system
   * @param ec the current execution context
   * @return an updated user akka behavior of type `Behavior[Command]`
   */
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
        state.polyBalance >= numberOfPolysRequiredForAssetOp,
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

  /**
   * Instantiates a user actor behavior with the given `UserState`.
   * @param state the user state to create the behavior with
   * @param networkPrefix the Bifrost network prefix
   * @param requestModifier the HTTP request modifier
   * @param timeout the amount of time to wait for actor responses
   * @param actorSystem the classic actor system for this typed system
   * @param ec the execution context
   * @return a user behavior of type `Behavior[Command]`
   */
  private def withState(state: UserState)(implicit
    networkPrefix:             NetworkPrefix,
    requestModifier:           RequestModifier,
    timeout:                   Timeout,
    actorSystem:               ActorSystem,
    ec:                        ExecutionContext
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

  def apply(addr:    Address, keys: ActorRef[KeysActor.Command], statsPath: String)(implicit
    networkPrefix:   NetworkPrefix,
    timeout:         Timeout,
    requestModifier: RequestModifier
  ): Behavior[Command] =
    Behaviors.setup { context =>
      val txActor = context.spawn(TransactionActor(keys, addr), s"TransactionActor_$addr")
      implicit val system: ActorSystem = context.system.classicSystem
      implicit val ec: ExecutionContext = context.executionContext

      withState(UserState(0, Map(), addr, List(), keys, txActor))
    }
}
