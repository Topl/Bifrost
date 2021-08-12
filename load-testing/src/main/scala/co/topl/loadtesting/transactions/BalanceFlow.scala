package co.topl.loadtesting.transactions

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.Flow
import co.topl.akkahttprpc.implicits.client._
import co.topl.akkahttprpc.{RequestModifier, RpcClientFailure}
import co.topl.attestation.Address
import co.topl.rpc.ToplRpc.NodeView.Balances
import co.topl.rpc.ToplRpc.NodeView.Balances.Response
import co.topl.rpc.implicits.client._
import co.topl.utils.NetworkType.NetworkPrefix

import scala.concurrent.ExecutionContext

object BalanceFlow {

  def apply()(implicit
    networkPrefix:    NetworkPrefix,
    classicSystem:    ActorSystem,
    requestModifier:  RequestModifier,
    executionContext: ExecutionContext
  ): Flow[Address, Either[RpcClientFailure, Response], NotUsed] =
    Flow[Address]
      .map(List(_))
      .map(Balances.Params)
      .mapAsync(1)(Balances.rpc(_).value)
}
