package co.topl.rpc

import akka.Done
import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import cats.data.EitherT
import cats.implicits._
import co.topl.akkahttprpc.RpcError
import co.topl.attestation.Address
import co.topl.consensus.Forger
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.nodeView.{CurrentView, NodeViewHolder}

import scala.concurrent.{ExecutionContext, Future}

package object handlers {

  import scala.language.implicitConversions

  type GetHistory = () => EitherT[Future, RpcError[_], History]
  type GetState = () => EitherT[Future, RpcError[_], State]
  type GetMempool = () => EitherT[Future, RpcError[_], MemPool]

  type ProcessTransaction = Transaction.TX => EitherT[Future, RpcError[_], Done]

  implicit def nodeViewHolderRefAsGetHistory(
    actorRef:         ActorRef
  )(implicit timeout: Timeout, ec: ExecutionContext): GetHistory = () =>
    EitherT
      .liftF(
        (actorRef ? NodeViewHolder.ReceivableMessages.GetDataFromCurrentView)
          .mapTo[CurrentView[History, _, _]]
      )
      .map(_.history)

  implicit def nodeViewHolderRefAsGetMempool(
    actorRef:         ActorRef
  )(implicit timeout: Timeout, ec: ExecutionContext): GetMempool = () =>
    EitherT
      .liftF(
        (actorRef ? NodeViewHolder.ReceivableMessages.GetDataFromCurrentView)
          .mapTo[CurrentView[_, _, MemPool]]
      )
      .map(_.pool)

  implicit def nodeViewHolderRefAsGetState(
                                            actorRef:         ActorRef
                                          )(implicit timeout: Timeout, ec: ExecutionContext): GetState = () =>
    EitherT
      .liftF(
        (actorRef ? NodeViewHolder.ReceivableMessages.GetDataFromCurrentView)
          .mapTo[CurrentView[_, State, _]]
      )
      .map(_.state)

  implicit def nodeViewHolderRefAsProcessTransaction(
                                            actorRef:         ActorRef
                                          )(implicit timeout: Timeout, ec: ExecutionContext): ProcessTransaction = tx =>
    EitherT
      .pure[Future, RpcError[_]](actorRef ! LocallyGeneratedTransaction(tx))
      .map(_ => Done)

  type ListKeys = () => EitherT[Future, RpcError[_], Set[Address]]

  implicit def forgerRefAsListKeys(actorRef: ActorRef)(implicit timeout: Timeout, ec: ExecutionContext): ListKeys =
    () => EitherT.liftF((actorRef ? Forger.ReceivableMessages.ListKeys).mapTo[Set[Address]])

  private[handlers] def checkAddresses(keys: List[Address], state: State): Either[RpcError[_], List[Address]] =
    for {
      _ <- Either.cond(state.hasTBR, {}, ToplRpcErrors.unsupportedOperation("TokenBoxRegistry not defined for node"))
      _ <- Either.cond(
        state.nodeKeys.forall(_.intersect(keys.toSet).nonEmpty),
        {},
        ToplRpcErrors.unsupportedOperation("TokenBoxRegistry not defined for node")
      )
    } yield keys

}
