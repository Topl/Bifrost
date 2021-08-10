package co.topl.loadtesting.transactions

import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import co.topl.akkahttprpc.RequestModifier
import co.topl.akkahttprpc.implicits.client._
import co.topl.modifier.ModifierId
import co.topl.rpc.ToplRpc.NodeView.TransactionById
import co.topl.rpc.implicits.client._
import co.topl.utils.NetworkType.NetworkPrefix

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

object TransactionTracker {
  def apply(wait: Int, txId: ModifierId)
           (implicit
            networkPrefix: NetworkPrefix,
            actorSystem: ActorSystem,
            requestModifier: RequestModifier,
            ec: ExecutionContext
           ): Future[(Boolean, Int)] =
    Source(1 to wait)
      .throttle(1, 1.second)
      .map(count => (count, TransactionById.Params(txId)))
      .mapAsync(1)(x => TransactionById.rpc(x._2).value.map((x._1, _)))
      .takeWhile(_._2.isLeft, inclusive = true)
      .runFold((false, 0))((_, x) => (x._2.isRight, x._1))
}
