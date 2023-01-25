package co.topl.loadtesting.user

import co.topl.rpc.implicits.client._
import co.topl.akkahttprpc.implicits.client._
import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import co.topl.akkahttprpc.RequestModifier
import co.topl.modifier.ModifierId
import co.topl.rpc.ToplRpc.NodeView.TransactionById
import co.topl.utils.NetworkType.NetworkPrefix

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

object TransactionTracker {

  sealed trait TransactionResult
  case class TransactionConfirmed(txId: ModifierId, seconds: Int) extends TransactionResult
  case class TransactionUnconfirmed(txId: ModifierId) extends TransactionResult

  def apply(wait: Int, txId: ModifierId)(implicit
    networkPrefix:   NetworkPrefix,
    actorSystem:     ActorSystem,
    requestModifier: RequestModifier,
    ec:              ExecutionContext
  ): Future[TransactionResult] =
    Source(LazyList.from(0))
      .throttle(1, 1.second)
      .map(count => (count, TransactionById.Params(txId)))
      .mapAsync(1)(x => TransactionById.rpc(x._2).value.map((x._1, _)))
      .takeWhile(_._2.isLeft, inclusive = true)
      .runFold(TransactionUnconfirmed(txId): TransactionResult) {
        case (x: TransactionConfirmed, _)          => x
        case (_, (time, result)) if result.isRight => TransactionConfirmed(txId, time)
        case _                                     => TransactionUnconfirmed(txId)
      }
}
