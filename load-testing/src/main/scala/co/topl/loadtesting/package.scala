package co.topl

import cats.implicits._
import akka.NotUsed
import akka.actor.ActorSystem
import akka.actor.typed.ActorRef
import akka.stream.scaladsl.{Flow, Sink}
import akka.stream.typed.scaladsl.ActorFlow
import akka.util.Timeout
import cats.Show
import co.topl.akkahttprpc.RequestModifier
import co.topl.rpc.ToplRpc.Transaction.BroadcastTx.Response
import co.topl.utils.NetworkType.NetworkPrefix

import scala.concurrent.ExecutionContext

package object loadtesting {

  def tryPolyTxFlow(userActor: ActorRef[PolyUserActor.Command])(implicit
    networkPrefix:             NetworkPrefix,
    requestModifier:           RequestModifier,
    timeout:                   Timeout,
    actorSystem:               ActorSystem,
    executionContext:          ExecutionContext
  ): Flow[NotUsed, Either[PolyUserActor.SendPolysFailure, Response], NotUsed] =
    Flow[NotUsed]
      .via(ActorFlow.ask(userActor)((_, r) => PolyUserActor.UpdateBalance(r)))
      .via(ActorFlow.ask(userActor)((_, r) => PolyUserActor.TrySendPolys(r)))

  def tryAssetTxFlow(userActor: ActorRef[AssetUserActor.Command])(implicit
    networkPrefix:              NetworkPrefix,
    requestModifier:            RequestModifier,
    timeout:                    Timeout,
    actorSystem:                ActorSystem,
    executionContext:           ExecutionContext
  ): Flow[NotUsed, Either[AssetUserActor.SendAssetsFailure, Response], NotUsed] =
    Flow[NotUsed]
      .via(ActorFlow.ask(userActor)((_, r) => AssetUserActor.UpdateBalance(r)))
      .via(ActorFlow.ask(userActor)((_, r) => AssetUserActor.TrySendAssets(r)))

  def logToConsoleSink[T: Show]: Sink[T, NotUsed] = Flow[T].map(_.show).to(Sink.foreach(println))
}
