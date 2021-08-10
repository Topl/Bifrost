package co.topl.loadtesting.transactions

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.Flow
import co.topl.akkahttprpc.implicits.client._
import co.topl.akkahttprpc.{RequestModifier, RpcClientFailure}
import co.topl.modifier.transaction.Transaction.TX
import co.topl.rpc.ToplRpc.Transaction.BroadcastTx
import co.topl.rpc.ToplRpc.Transaction.BroadcastTx.Response
import co.topl.rpc.implicits.client._
import co.topl.utils.NetworkType.NetworkPrefix

import scala.concurrent.ExecutionContext

object BroadcastFlow {

  def apply()(
            implicit requestModifier: RequestModifier,
            ec: ExecutionContext, materializer: Materializer,
            classicSystem: ActorSystem,
            networkPrefix: NetworkPrefix
          ): Flow[TX, Either[RpcClientFailure, Response], NotUsed] =
    Flow[TX]
      .map(BroadcastTx.Params)
      .mapAsync(1)(BroadcastTx.rpc(_).value)
}
