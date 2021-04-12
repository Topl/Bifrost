package co.topl.rpc

import akka.Done
import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import cats.data.EitherT
import cats.implicits._
import co.topl.akkahttprpc.{CustomError, RpcError, ThrowableData}
import co.topl.attestation.Address
import co.topl.consensus.Forger
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.nodeView.{CurrentView, NodeViewHolder}
import io.circe.Encoder
import spire.ClassTag

import scala.concurrent.{ExecutionContext, Future}

package object handlers {

  import scala.language.implicitConversions

  type GetHistory = () => EitherT[Future, RpcError, History]
  type GetState = () => EitherT[Future, RpcError, State]
  type GetMempool = () => EitherT[Future, RpcError, MemPool]

  type ProcessTransaction = Transaction.TX => EitherT[Future, RpcError, Done]

  implicit def nodeViewHolderRefAsGetHistory(
    actorRef:         ActorRef
  )(implicit timeout: Timeout, ec: ExecutionContext, encodeThrowableData: Encoder[ThrowableData]): GetHistory = () =>
    actorAskToEitherT[CurrentView[History, _, _]](actorRef, NodeViewHolder.ReceivableMessages.GetDataFromCurrentView)
      .map(_.history)

  implicit def nodeViewHolderRefAsGetMempool(
    actorRef:         ActorRef
  )(implicit timeout: Timeout, ec: ExecutionContext, encodeThrowableData: Encoder[ThrowableData]): GetMempool = () =>
    actorAskToEitherT[CurrentView[_, _, MemPool]](actorRef, NodeViewHolder.ReceivableMessages.GetDataFromCurrentView)
      .map(_.pool)

  implicit def nodeViewHolderRefAsGetState(
    actorRef:         ActorRef
  )(implicit timeout: Timeout, ec: ExecutionContext, encodeThrowableData: Encoder[ThrowableData]): GetState = () =>
    actorAskToEitherT[CurrentView[_, State, _]](actorRef, NodeViewHolder.ReceivableMessages.GetDataFromCurrentView)
      .map(_.state)

  implicit def nodeViewHolderRefAsProcessTransaction(
    actorRef:         ActorRef
  )(implicit timeout: Timeout, ec: ExecutionContext): ProcessTransaction = tx =>
    EitherT
      .pure[Future, RpcError](actorRef ! LocallyGeneratedTransaction(tx))
      .map(_ => Done)

  type ListKeys = () => EitherT[Future, RpcError, Set[Address]]

  implicit def forgerRefAsListKeys(
    actorRef:         ActorRef
  )(implicit timeout: Timeout, ec: ExecutionContext, encodeThrowableData: Encoder[ThrowableData]): ListKeys =
    () => actorAskToEitherT[Set[Address]](actorRef, Forger.ReceivableMessages.ListKeys)

  private[handlers] def checkAddresses(keys: List[Address], state: State): Either[RpcError, List[Address]] =
    for {
      _ <- Either.cond(state.hasTBR, {}, ToplRpcErrors.unsupportedOperation("TokenBoxRegistry not defined for node"))
      _ <- Either.cond(
        state.nodeKeys.forall(_.intersect(keys.toSet).nonEmpty),
        {},
        ToplRpcErrors.unsupportedOperation("TokenBoxRegistry not defined for node")
      )
    } yield keys

  private[handlers] def actorAskToEitherT[ReturnType : ClassTag](actorRef: ActorRef, message: Any)(implicit
                                                                                                   timeout:                                                    Timeout,
                                                                                                   ec:                                                         ExecutionContext,
                                                                                                   encodeThrowableData:                                        Encoder[ThrowableData]
  ): EitherT[Future, RpcError, ReturnType] =
    EitherT(
      (actorRef ? NodeViewHolder.ReceivableMessages.GetDataFromCurrentView)
        .mapTo[ReturnType]
        .map(_.asRight)
        .recover { case e => CustomError.fromThrowable(e).asLeft }
    )

}
