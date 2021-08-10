package co.topl.loadtesting

import akka.NotUsed
import akka.actor.ActorSystem
import akka.actor.typed.ActorRef
import akka.stream.scaladsl.Flow
import akka.stream.typed.scaladsl.ActorFlow
import akka.util.Timeout
import co.topl.akkahttprpc.RequestModifier
import co.topl.loadtesting.PolyUserActor.SendPolysFailure
import co.topl.rpc.ToplRpc.Transaction.BroadcastTx.Response
import co.topl.utils.NetworkType.NetworkPrefix

import scala.concurrent.ExecutionContext

object TrySendPolysFlow {

  val transactionTrackingTimeout = 60

  private def sendTransactionFlow(
    userActor:        ActorRef[PolyUserActor.Command]
  )(implicit timeout: Timeout): Flow[NotUsed, Either[SendPolysFailure, Response], NotUsed] =
    Flow[NotUsed]
      .via(ActorFlow.ask(userActor)((_, r) => PolyUserActor.UpdateBalance(r)))
      .via(ActorFlow.ask(userActor)((_, r) => PolyUserActor.TrySendPolys(r)))

  private def txFlow(userActor: ActorRef[PolyUserActor.Command])(implicit
    timeout:                    Timeout,
    actorSystem:                ActorSystem,
    requestModifier:            RequestModifier,
    executionContext:           ExecutionContext,
    networkPrefix:              NetworkPrefix
  ): Flow[NotUsed, Either[SendPolysFailure, Response], NotUsed] =
    Flow[NotUsed]
      .via(sendTransactionFlow(userActor))

  def apply(userActor: ActorRef[PolyUserActor.Command])(implicit
    networkPrefix:     NetworkPrefix,
    requestModifier:   RequestModifier,
    timeout:           Timeout,
    actorSystem:       ActorSystem,
    executionContext:  ExecutionContext
  ): Flow[NotUsed, Either[SendPolysFailure, Response], NotUsed] = txFlow(userActor)
}
